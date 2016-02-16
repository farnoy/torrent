{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
module PeerMonadSpec where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception.Base as Exception
import qualified Control.Monad.Catch as Catch
import Control.Monad.Catch.Pure
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Free.Church
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Functor.Identity
import Data.IntSet (IntSet)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Time.Clock
import Data.Word
import Network.BitTorrent.Bencoding
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.Client
import qualified Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.DownloadProgress as DP
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.MemoryMonad
import Network.BitTorrent.PeerMonad
import qualified Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Types
import Network.BitTorrent.Utility
import System.Directory
import System.IO
import System.Random
import System.Timeout

import Test.Hspec
import SpecHelper

data Memory = Memory { memoryBitField :: BF.BitField
                     , memoryPieceChunks :: Chunks
                     -- , memoryAvailabilityData :: PS.AvailabilityData
                     , memoryRequestablePieces :: IntSet
                     }

class TorrentStateDumper t where
  clientStateToMemory :: TorrentState t -> IO Memory

instance TorrentStateDumper 'Production where
  clientStateToMemory state = atomically (Memory
                                             <$> readTVar (torrentStateBitField state)
                                             <*> readPieceChunks state
                                             -- <*> readTVar (availabilityData state)
                                             <*> readTVar (torrentStateRequestablePieces state))

    where readPieceChunks :: TorrentState 'Production -> STM Chunks
          readPieceChunks state =
            fmap (Map.fromList . convert) (traverse lookup' [PieceId k | k <- [0..(pieceCount - 1)]])
            where pieceCount = fromIntegral . (`quot` 20) . B.length . pieces . info . torrentStateMetaInfo $ state
                  lookup' :: PieceId -> STM (Maybe (PieceId, CF.ChunkField))
                  lookup' k = fmap (fmap (k,)) (DP.lookup k (torrentStateDownloadProgress state))
                  convert :: [Maybe (PieceId, CF.ChunkField)] -> [(PieceId, CF.ChunkField)]
                  convert = foldl f []
                  f accu (Just assoc) = assoc : accu
                  f accu Nothing = accu

type MemoryMonadTest = StateT Memory Identity

runMemoryMonadTest :: Memory -> F MemoryMonad a -> (a, Memory)
runMemoryMonadTest mem t = runIdentity (runStateT (iterM evalMemoryMonadTest t) mem)

evalMemoryMonadTest :: MemoryMonad (MemoryMonadTest a) -> MemoryMonadTest a
{-
evalMemoryMonadTest (ModifyAvailability mut next) = do
  memory <- get
  put $ memory { memoryAvailabilityData = mut (memoryAvailabilityData memory) }
  next
evalMemoryMonadTest (ReadAvailability next) = do
  memory <- get
  next $ memoryAvailabilityData memory
  -}
evalMemoryMonadTest (ReadBitfield next) = do
  memory <- get
  next $ memoryBitField memory
evalMemoryMonadTest (GetDownloadProgress piece next) = do
  memory <- get
  next $ Map.lookup piece (memoryPieceChunks memory)
evalMemoryMonadTest (SetDownloadProgress piece cf next) = do
  memory <- get
  put $ memory { memoryPieceChunks = Map.insert piece cf (memoryPieceChunks memory) }
  next
evalMemoryMonadTest (RemoveDownloadProgress piece next) = do
  memory <- get
  put $ memory { memoryPieceChunks = Map.delete piece (memoryPieceChunks memory) }
  next
evalMemoryMonadTest (ReadRequestablePieces next) = do
  memory <- get
  next $ memoryRequestablePieces memory
evalMemoryMonadTest (ModifyRequestablePieces mut next) = do
  memory <- get
  put $ memory { memoryRequestablePieces =  mut $ memoryRequestablePieces memory }
  next

data PeerState = PeerState { peerStateData :: PeerData
                           , peerStateOutputs :: [PWP]
                           , peerStateActiveChunks :: Map.Map (PieceId, ChunkId) UTCTime
                           , peerStateEvents :: [PeerEvent]
                           , peerStateMemory :: Memory
                           , peerStateCurrentTime :: UTCTime
                           }

type PeerMonadTest t = CatchT (ReaderT (TorrentState t) (StateT PeerState Identity))

runPeerMonadTest :: TorrentState t
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

evalPeerMonadTest :: PeerMonad (PeerMonadTest t a) -> PeerMonadTest t a
evalPeerMonadTest (GetPeerData next) = do
  PeerState pData _ _ _ _ _ <- get
  next pData
evalPeerMonadTest (Emit msg next) = do
  pState@(PeerState _ outputs _ _ _ _) <- get
  put $ pState { peerStateOutputs = msg : outputs }
  next
evalPeerMonadTest (GetMeta next) = torrentStateMetaInfo <$> ask >>= next
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
evalPeerMonadTest (GetActiveChunks next) = do
  pState <- get
  next $ peerStateActiveChunks pState
evalPeerMonadTest (RegisterActiveChunk pid cid next) = do
  pState <- get
  let t = peerStateCurrentTime pState
  put $ pState { peerStateCurrentTime = addUTCTime 1 t
               , peerStateActiveChunks = Map.insert (pid, cid) t (peerStateActiveChunks pState)
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
evalPeerMonadTest (Log exp next) = next

spec :: SpecWith ()
spec = do
  tmpdir <- runIO getTemporaryDirectory
  handle <- runIO $ openFile "/dev/null" WriteMode
  refTime <- runIO $ getCurrentTime
  let addr = testAddr [1, 0, 0, 127] 9999
      peerId = B.replicate (fromEnum '1') 20
      bf = BF.BitField (B.replicate (quot pieceCount 8) maxBound) pieceCount
      pData = newPeer bf addr peerId
      withState f = do
        n <- randomIO :: IO Word16
        let dirName = tmpdir <> "/" <> show n
        createDirectory dirName
        state <- newTorrentState dirName testMeta
        memory <- clientStateToMemory state
        Exception.finally (f (state, memory)) (removeDirectoryRecursive dirName)

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
                catchError (registerActiveChunk (PieceId 0) (ChunkId 0) *> getPeerEvent *> pure [])
                           (const $ getActiveChunks >>= pure . Map.keys)
              res = fst <$> runPeerMonadTest state pData memory events exp refTime
          res `shouldBe` Right [(PieceId 0, ChunkId 0)]

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
