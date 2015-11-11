{-# LANGUAGE OverloadedStrings #-}
module PeerMonadSpec where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception.Base
import qualified Control.Monad.Catch as Catch
import Control.Monad.Catch.Pure
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Time.Clock
import Data.Word
import Network.BitTorrent.Bencoding
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.MemoryMonad
import Network.BitTorrent.PeerMonad
import qualified Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Types
import System.Directory
import System.IO
import System.Random
import System.Timeout

import Test.Hspec
import SpecHelper

data Memory = Memory { memoryBitField :: BF.BitField
                     , memoryPieceChunks :: Chunks
                     , memoryAvailabilityData :: PS.AvailabilityData
                     }

clientStateToMemory :: ClientState -> IO Memory
clientStateToMemory state = atomically (Memory
                                           <$> readTVar (bitField state)
                                           <*> readTVar (pieceChunks state)
                                           <*> readTVar (availabilityData state))

type MemoryMonadTest = StateT Memory Identity

runMemoryMonadTest :: Memory -> F MemoryMonad a -> (a, Memory)
runMemoryMonadTest mem t = runIdentity (runStateT (iterM evalMemoryMonadTest t) mem)

evalMemoryMonadTest :: MemoryMonad (MemoryMonadTest a) -> MemoryMonadTest a
evalMemoryMonadTest (ModifyAvailability mut next) = do
  memory <- get
  put $ memory { memoryAvailabilityData = mut (memoryAvailabilityData memory) }
  next
evalMemoryMonadTest (ReadAvailability next) = do
  memory <- get
  next $ memoryAvailabilityData memory
evalMemoryMonadTest (ReadBitfield next) = do
  memory <- get
  next $ memoryBitField memory
evalMemoryMonadTest (GetChunks next) = do
  memory <- get
  next $ memoryPieceChunks memory
evalMemoryMonadTest (ModifyChunks mut next) = do
  memory <- get
  put $ memory { memoryPieceChunks =  mut $ memoryPieceChunks memory }
  next

data PeerState = PeerState { peerStateData :: PeerData
                           , peerStateOutputs :: [PWP]
                           , peerStateCleanups :: Map.Map (Word32, Word32) UTCTime
                           , peerStateEvents :: [PeerEvent]
                           , peerStateMemory :: Memory
                           , peerStateCurrentTime :: UTCTime
                           }

type PeerMonadTest = CatchT (ReaderT ClientState (StateT PeerState Identity))

runPeerMonadTest :: ClientState
                 -> PeerData
                 -> Memory
                 -> [PeerEvent]
                 -> F PeerMonad a
                 -> UTCTime
                 -> Either (Maybe PeerError) (a, PeerState)
runPeerMonadTest state pData memory events t refTime =
  case result of
    (Left e, _) -> Left (fromException e)
    (Right a, s) -> Right (a, s)
  where peerState = PeerState pData [] Map.empty events memory refTime
        result = runIdentity (
                   runStateT (
                     runReaderT (
                       runCatchT (
                         iterM evalPeerMonadTest t
                       )
                     )
                     state
                   )
                   peerState
                 )

evalPeerMonadTest :: PeerMonad (PeerMonadTest a) -> PeerMonadTest a
evalPeerMonadTest (GetPeerData next) = do
  PeerState pData _ _ _ _ _ <- get
  next pData
evalPeerMonadTest (Emit msg next) = do
  pState@(PeerState _ outputs _ _ _ _) <- get
  put $ pState { peerStateOutputs = msg : outputs }
  next
evalPeerMonadTest (GetMeta next) = metaInfo <$> ask >>= next
evalPeerMonadTest (UpdatePeerData pData next) = do
  pState <- get
  put $ pState { peerStateData = pData }
  next
evalPeerMonadTest (GetPeerEvent next) = do
  pState@(PeerState _ _ _ events _ _) <- get
  case events of
    [] -> Catch.throwM ConnectionLost
    (event:rest) -> do
      put $ pState { peerStateEvents = rest }
      next event
evalPeerMonadTest (RunMemory action next) = do
  pState <- get
  let (res, mem') = runMemoryMonadTest (peerStateMemory pState) action
  put $ pState { peerStateMemory = mem' }
  next res
evalPeerMonadTest (GetCleanups next) = do
  pState <- get
  next $ peerStateCleanups pState
evalPeerMonadTest (RegisterCleanup pid cid next) = do
  pState <- get
  let t = peerStateCurrentTime pState
  put $ pState { peerStateCurrentTime = addUTCTime 1 t
               , peerStateCleanups = Map.insert (pid, cid) t (peerStateCleanups pState)
               }
  next
evalPeerMonadTest (Throw e next) = Catch.throwM e *> next
evalPeerMonadTest (Catch action handler) =
  Catch.catch action handler
evalPeerMonadTest (GetTime next) = do
  pState <- get
  let t = peerStateCurrentTime pState
  put $ pState { peerStateCurrentTime = addUTCTime 1 t }
  next t

spec :: SpecWith ()
spec = do
  tmpdir <- runIO getTemporaryDirectory
  handle <- runIO $ openFile "/dev/null" WriteMode
  refTime <- runIO $ getCurrentTime
  let addr = testAddr [1, 0, 0, 127] 9999
      peerId = B.replicate (fromEnum '1') 20
      bf = BF.BitField (B.replicate (1 + (quot pieceCount 8)) maxBound) pieceCount
      pData = newPeer bf addr peerId
      withState :: ((ClientState, Memory) -> IO ()) -> IO ()
      withState f = do
        n <- randomIO :: IO Int
        let testMeta' = testMeta
              { info = (info testMeta)
                { name = "test" <> BC.pack (show n) }
              }
        state <- newClientState tmpdir testMeta' 9999
        memory <- clientStateToMemory state
        f (state, memory)

  describe "PeerMonad" $ around withState $ do
    describe "handlePWP" $ do
      describe "for Have message" $ do
        it "properly adjust the peer bitfield" $ \(state, memory) -> do
          let exp = handlePWP (Have 2) *> getPeerData
              Right (pData', _) = runPeerMonadTest state pData memory [] exp refTime
          BF.get (peerBitField pData') 2 `shouldBe` True

    describe "exception handling" $ do
      it "catches errors from monad evaluation" $ \(state, memory) -> do
        let exp = catchError (getPeerEvent *> pure 0) (const $ pure 2)
            res = fst <$> runPeerMonadTest state pData memory [] exp refTime
        res `shouldBe` Right 2

    describe "requestNextChunk" $ do
      it "is a noop when peer is choking us" $ \(state, memory) -> do
        let exp = requestNextChunk
            res = runPeerMonadTest state pData memory [] exp refTime
            peerState = snd <$> res

        peerStateOutputs <$> peerState `shouldBe` Right []

      it "is a noop when live requests exceed the limit" $ \(state, memory) -> do
        let exp = requestNextChunk
            pData' = pData { peerChoking = False, requestsLive = maxRequestsPerPeer }
            res = runPeerMonadTest state pData' memory [] exp refTime
            peerState = snd <$> res

        null . peerStateOutputs <$> peerState `shouldBe` Right True

      it "requests the next first piece" $ \(state, memory) -> do
        let exp = requestNextChunk
            pData' = pData { peerChoking = False }
            res = runPeerMonadTest state pData' memory [] exp refTime
            peerState = snd <$> res

        fst <$> res `shouldBe` Right ()
        elem (Request 0 0 (2^14)) . peerStateOutputs <$> peerState `shouldBe` Right True

  describe "PeerMonadTest" $ around withState $ do
    describe "exception handling" $ do
      describe "on getPeerEvent" $ do
        it "throws ConnectionLost after all messages" $ \(state, memory) -> do
          let events = [PWPEvent KeepAlive, SharedEvent Checkup]
              exp = do
                res  <- sequence (replicate 2 getPeerEvent)
                res2 <- catchError (getPeerEvent *> pure False) (const $ pure True)
                return (res, res2)
              res = fst <$> runPeerMonadTest state pData memory events exp refTime
          res `shouldBe` Right (events, True)

      it "preserves cleanups for the handler" $ \(state, memory) -> do
          let events = []
              exp =
                catchError (registerCleanup 0 0 *> getPeerEvent *> pure [])
                           (const $ getCleanups >>= pure . Map.keys)
              res = fst <$> runPeerMonadTest state pData memory events exp refTime
          res `shouldBe` Right [(0, 0)]

      it "preserves peer data for the handler" $ \(state, memory) -> do
          let events = []
              exp = do
                catchError (do
                  pData' <- getPeerData
                  updatePeerData $ pData' { requestsLive = 5 }
                  throwError ConnectionLost
                  return 0)
                           (const $ getPeerData >>= pure . requestsLive)
              res = fst <$> runPeerMonadTest state pData memory events exp refTime
          res `shouldBe` Right 5

  describe "PeerMonadIO" $ around withState $ do
    describe "local state manipulation" $ do
      it "respects state updates after exceptions" $ \(state, _) -> do
        let pData' = pData { amChoking = False }
            exp = catchError (do
                    updatePeerData pData'
                    throwError ConnectionLost
                    return pData)
                             (const $ getPeerData)
        returned <- runPeerMonad state pData handle exp
        returned `shouldBe` Right pData'

      it "respects state updates" $ \(state, _) -> do
        let pData' = pData { amChoking = False }
            exp = updatePeerData pData' *> getPeerData
        returned <- runPeerMonad state pData handle exp
        returned `shouldBe` Right pData'
