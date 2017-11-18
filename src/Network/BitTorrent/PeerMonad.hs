{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Provides definiton for 'PeerMonad' and an IO based implementation
-- for actual connections to use in the real world.
--
-- Expressions in the 'PeerMonad' use a number of backend-implemented
-- operations to implement their logic.
-- The main expression in this module is 'entryPoint' - the peer loop.
-- It evaluates events, interacts with the peer and uses shared memory
-- to coordinate with other peer loops.
module Network.BitTorrent.PeerMonad (
-- * PeerMonad
  MonadPeer(..)
, PeerEvent(..)
, handlePWP
, runPeerMonad
, PeerError(..)
, ActiveChunks

-- * Actions
, entryPoint
, requestNextChunk
, nextRequestOperation
, receiveChunk
, RequestOperation(..)

-- * Operations
, runMemory
, getPeerData
, emit
, getMeta
, readData
, writeData
, updatePeerData
, getPeerEvent
, registerActiveChunk
, deregisterActiveChunk
, releaseActiveChunk
, getActiveChunks
, getTime
, catchError
, throwError
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except as Except
import Control.Monad.Identity
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Strict
import Control.Monad.Trans.Free.Church
import Crypto.Hash.SHA1
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as B
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Internal as BI
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word
import Flow
-- import Data.Vector.Storable.Mutable as VS
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.FileWriter as FW
import qualified Network.BitTorrent.LinkSpeed as LS
import Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.MemoryMonad
-- import Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Utility
import Network.BitTorrent.Types
import Prelude hiding (log)
import System.IO

-- | Maps (PieceId, ChunkId) pair with a timestamp.
--
-- It's used to store when a request for a chunk was made
-- so we can time out if a peer has not responded for long enough.
--
-- Comes in handy when the connection to a peer is lost -
-- we can then revert the downloads of chunks by marking
-- them as missing again.
type ActiveChunks = Map (PieceId, ChunkId) UTCTime

data PeerState = PeerState { peerStateData :: !PeerData
                           , peerStateChan :: Chan PeerEvent
                           , peerStateHandle :: Handle
                           , peerStateActiveChunks :: ActiveChunks
                           }

-- | Encodes exceptions that can happen in a peer loop.
data PeerError = ConnectionLost
               | AssertionFailed String
               deriving(Eq,Show)
instance Exception PeerError

-- TODO Logger
type PeerMonadIO = ExceptT PeerError (ReaderT TorrentState (StateT PeerState (LoggingT IO)))

-- | Encodes possible events that arrive in the 'PeerMonad'.
--
-- Retrieved by using 'getPeerEvent'.
data PeerEvent = PWPEvent PWP
               | SharedEvent SharedMessage
               | ErrorEvent PeerError
               deriving(Eq,Show)

class (MonadLogger m, MonadError PeerError m) => MonadPeer m where
  runMemory :: (F MemoryMonad b) -> m b
  emit :: PWP -> m ()
  getPeerData :: m PeerData
  updatePeerData :: PeerData -> m ()
  getMeta :: m MetaInfo
  getTime :: m UTCTime
  readData :: Word64 -> Word64 -> m ByteString
  writeData :: Word64 -> ByteString -> m ()
  getPeerEvent :: m PeerEvent
  registerActiveChunk :: PieceId -> ChunkId -> m ()
  deregisterActiveChunk :: PieceId -> ChunkId -> m ()
  getActiveChunks :: m ActiveChunks

instance MonadPeer PeerMonadIO where
  getMeta = torrentStateMetaInfo <$> ask
  getTime = liftIO getCurrentTime
  getPeerData = get >>= pure . peerStateData
  updatePeerData pData = do
    pState <- get
    put $ pState { peerStateData = pData }
  emit pwp = do
    state <- get
    let handle = peerStateHandle state

    res <- liftIO $ Catch.catchIOError
              (BL.hPut handle (Binary.encode pwp) *> return True)
              (const (return True))

    unless res $ Except.throwError ConnectionLost
  runMemory exp = do
    state <- ask
    liftIO $ atomically $ runMemoryMonadSTM state exp
  readData o l = do
    state <- ask
    let hdls = torrentStateOutputHandles state
        lock = torrentStateOutputLock state
    liftIO $ FW.read hdls lock o l
  writeData o d = do
    state <- ask
    let hdls = torrentStateOutputHandles state
        lock = torrentStateOutputLock state
    liftIO $ FW.write hdls lock o d
  getPeerEvent = do
    pState <- get
    liftIO $ readChan $ peerStateChan pState
  registerActiveChunk pieceId chunkId = do
    pState <- get

    t <- liftIO getCurrentTime
    put $ pState { peerStateActiveChunks = force $
                  Map.insert (pieceId, chunkId)
                              t
                              (peerStateActiveChunks pState) }
  deregisterActiveChunk pieceId chunkId = do
    pState <- get
    let activeChunks = peerStateActiveChunks pState

    put $ pState { peerStateActiveChunks = force $ Map.delete (pieceId, chunkId) activeChunks }
  getActiveChunks = get >>= pure . peerStateActiveChunks

log :: (MonadPeer m, MonadLogger m) => (T.Text -> m ()) -> m ()
log exp = do
  pData <- getPeerData
  case fromByteString (B64.encode (peerId pData)) of
    Just t -> exp t
    Nothing -> return ()



-- | Runs a 'PeerMonad' in the IO monad.
--
-- Internally spawns threads to process events from multiple sources efficiently.
-- It forwards 'SharedMessage's and 'PWP' messages to the 'PeerMonad' to act upon.
--
-- Nested 'MemoryMonad' expressions are evaluated using STM.
runPeerMonad :: TorrentState
             -> PeerData
             -> Handle -- ^ socket handle to communicate with the peer
             -> PeerMonadIO a
             -> IO (Either PeerError a)
runPeerMonad state pData outHandle t = do
  privateChan <- newChan
  sharedChan <- dupChan (torrentStateSharedMessages state)

  let peerState = PeerState pData privateChan outHandle Map.empty

  promise1 <- async $ messageForwarder outHandle privateChan
  promise2 <- async $ forever $ readChan sharedChan >>= writeChan privateChan . SharedEvent

  myId <- myThreadId
  atomically $ modifyTVar' (torrentStatePeerThreads state) (Seq.|> (myId, peerId pData))

  let action = runStderrLoggingT (filterLogger filter (evalStateT (runReaderT (runExceptT t) state) peerState))
      filter _ LevelDebug = False
      filter _ _ = True
      cleanup = do
        cancel promise1
        cancel promise2
        hClose outHandle
        atomically $ modifyTVar' (torrentStatePeerThreads state)
                                 (Seq.filter ((/= myId) . fst))

  action `finally` cleanup

  where
    messageForwarder handle privateChan = (do
      messageStream handle (writeChan privateChan . PWPEvent))
      `onException` writeChan privateChan (ErrorEvent ConnectionLost)
    messageStream :: Handle -> (PWP -> IO ()) -> IO ()
    messageStream hdl act = go initial
      where initial = Binary.runGetIncremental Binary.get
            bufSize = (2 :: Int) ^ (14 :: Int)
            go (Binary.Fail {}) = return ()
            go (Binary.Partial next) = do
              hWaitForInput hdl (-1)
              B.hGetSome hdl bufSize >>= go . next . Just
            go (Binary.Done unused _ value) =
              act value *> go (initial `Binary.pushChunk` unused)

recordDownloaded :: MonadPeer m => LS.Bytes -> m ()
recordDownloaded b = do
  t <- getTime
  let seconds = fromIntegral (utctDayTime t |> fromEnum) `quot` truncate (1e12 :: Float)
  runMemory $ liftF $ RecordDownloaded seconds b ()

recordUploaded :: MonadPeer m => LS.Bytes -> m ()
recordUploaded b = do
  t <- getTime
  let seconds = fromIntegral (utctDayTime t |> fromEnum) `quot` truncate (1e12 :: Float)
  runMemory $ liftF $ RecordUploaded seconds b ()

receiveChunk :: MonadPeer m => PieceId -> Word32 -> ByteString -> m ()
receiveChunk piece offset d = do
  let chunkIndex = ChunkId (divideSize offset defaultChunkSize)

  wasMarked <- runMemory $ {-# SCC "receiveChunk--memory" #-} do
    dp <- getDownloadProgress piece
    case dp of
      Just chunkField -> do
        let chunkField' = CF.markCompleted chunkField chunkIndex
        setDownloadProgress piece (force chunkField')
        return True
      _ -> return False -- someone already filled this

  deregisterActiveChunk piece chunkIndex
  pData <- getPeerData
  updatePeerData (pData { requestsLive = requestsLive pData - 1 })

  when wasMarked $ do
    recordDownloaded (B.length d |> fromIntegral)
    meta <- getMeta
    let infoDict = info meta
        defaultPieceLen = pieceLength infoDict
        PieceId pix = piece
    writeData (fromIntegral pix * fromIntegral defaultPieceLen + fromIntegral offset) d
    processPiece piece

processPiece :: MonadPeer m => PieceId -> m ()
processPiece piece@(PieceId pieceId) = do
  meta <- getMeta
  let infoDict = info meta
      defaultPieceLen = pieceLength infoDict
      totalSize = sum (Meta.length <$> Meta.files infoDict)
      pieceSize = expectedPieceSize totalSize defaultPieceLen piece

  isCompleted <- runMemory (fmap (fmap CF.isCompleted) (getDownloadProgress piece))

  when (isCompleted == Just True) $ do
    d <- readData (fromIntegral pieceId * fromIntegral defaultPieceLen) (fromIntegral pieceSize)

    dataToWrite <- runMemory $ do
      dp <- getDownloadProgress piece
      case dp of
        Just chunkField -> do
          let getPieceHash (PieceId n) =
                B.take 20 $ B.drop (fromIntegral n * 20) $ pieces infoDict

          when (CF.isRequested chunkField) $
            modifyRequestablePieces (IntSet.delete $ fromIntegral pieceId)

          if CF.isCompleted chunkField
            then do
              removeDownloadProgress piece
              if hash d == getPieceHash piece
                then do
                  bitfield <- getBitfield
                  let newBitfield = BF.set bitfield pieceId True
                  modifyBitfield (const newBitfield)
                  return (Just d)
                else do
                  modifyRequestablePieces (IntSet.insert $ fromIntegral pieceId)
                  return $ Just "hashFail"
                    -- we remove it from Chunks
                    -- but do not modify the bitfield,
                    -- it will be reacquired again
            else return Nothing
        Nothing -> return Nothing

    case dataToWrite of
      Just "hashFail" ->
        log $ \peerid -> logDebugN $ "Hashfail " <> peerid <> " " <> T.pack (show pieceId)
      Just d -> {-# SCC "processPiece--writeData" #-} writeData (fromIntegral defaultPieceLen * fromIntegral pieceId) d
      Nothing -> return ()

handleBitfield :: MonadPeer m => ByteString -> m ()
handleBitfield field = do
  peerData <- getPeerData
  newBitField <- runMemory $ do
    len <- BF.length <$> getBitfield
    let newBitField = BF.BitField field len
    -- modifyAvailability $ PS.addToAvailability newBitField
    return newBitField
  updatePeerData $ peerData { peerBitField = newBitField }
  emit Interested

handleHave :: MonadPeer m => PieceId -> m ()
handleHave (PieceId ix) = do
  peerData <- getPeerData
  let oldBf = peerBitField peerData
      newBf = BF.set oldBf ix True
      peerData' = peerData { peerBitField = newBf }
  {-
  runMemory $
    modifyAvailability $ PS.addToAvailability newBf . PS.removeFromAvailability oldBf
    -}
  updatePeerData peerData'

handleInterested :: MonadPeer m => m ()
handleInterested = do
  peerData <- getPeerData
  when (amChoking peerData) $ do
    emit Unchoke
    updatePeerData $ peerData { amChoking = False, peerInterested = True }

data RequestOperation = RequestChunk PieceId ChunkId PWP
                      | Raise PeerError

claimChunk :: Word64
           -> Word32
           -> CF.ChunkField
           -> PieceId
           -> F MemoryMonad (Maybe RequestOperation)
claimChunk totalSize pieceLen chunkField piece@(PieceId pieceId) =
  case CF.getNextChunk chunkField of
    Just (chunkField', chunk@(ChunkId chunkId)) -> do
      let request = Request pieceId
                            (chunkId * defaultChunkSize)
                            (nextChunkSize piece chunk)
      setDownloadProgress piece (force chunkField')
      when (CF.isRequested chunkField') $
        modifyRequestablePieces (IntSet.delete $ fromIntegral pieceId)
      return $ Just $ RequestChunk piece chunk request
    _ -> return Nothing
  where nextChunkSize :: PieceId -> ChunkId -> Word32
        nextChunkSize =
          expectedChunkSize totalSize pieceLen defaultChunkSize

nextRequestOperation :: PeerData -> MetaInfo -> F MemoryMonad (Maybe RequestOperation)
nextRequestOperation peerData meta = do
  requestablePieces <- getRequestablePieces

  let peersBitField = peerBitField peerData
      infoDict = info meta
      defaultPieceLen = pieceLength infoDict
      totalSize = {-# SCC "totalSize" #-} force $ sum (Meta.length <$> Meta.files infoDict)
      {-
      findPiece n | n == (BF.length ourBitField) = pure Nothing
      findPiece n = case IntSet.lookupGE (fromIntegral n) requestablePieces of
        Just k -> do
          let piece = PieceId (fromIntegral k)
              next = findPiece (fromIntegral k + 1)
          dp <- getDownloadProgress piece
          case dp of
            Nothing -> pure $ Just piece
            Just chunkField -> case CF.getNextChunk chunkField of
              Just _ -> pure $ Just piece
              _ -> next
        _ -> pure Nothing
      -}
      findPieceSimple =
        case IntSet.lookupGE 0 requestablePieces of
          Just k -> Just $ PieceId $ fromIntegral k
          _ -> Nothing

      nextPiece = findPieceSimple
  -- nextPiece <- findPiece 0

  case nextPiece of
    Nothing -> return Nothing
    Just piece@(PieceId pieceId) ->
      let pieceLen = expectedPieceSize totalSize defaultPieceLen piece
      in case () of
        _ | not (BF.get peersBitField pieceId) -> do
          ourBitField <- getBitfield
          return (Just $ Raise $ AssertionFailed ("Peer does not have it, peer:" ++ show (BF.get peersBitField pieceId) ++ ", us:" ++ show (BF.get ourBitField pieceId)))
        _ -> do
          dp <- getDownloadProgress piece
          case dp of
            Nothing -> do
              let chunksCount = chunksInPiece pieceLen defaultChunkSize
                  chunkField = CF.newChunkField (fromIntegral chunksCount)
              claimChunk totalSize pieceLen (force chunkField) piece
            Just chunkInfo -> claimChunk totalSize pieceLen chunkInfo piece

requestNextChunk :: MonadPeer m => m ()
requestNextChunk = do
  peerData <- getPeerData
  meta <- getMeta
  unless (peerDataStopping peerData) $
    when (requestsLive peerData < maxRequestsPerPeer) $
    unless (peerChoking peerData) $ do
      operation <- runMemory $ nextRequestOperation peerData meta

      case operation of
        Just (RequestChunk pieceId chunkId pwpRequest) -> do
          registerActiveChunk pieceId chunkId
          let modifiedPeer = peerData { requestsLive = requestsLive peerData + 1 }
          updatePeerData modifiedPeer
          emit pwpRequest
          requestNextChunk
        Just (Raise err) -> throwError err
        Nothing -> return ()

handlePWP :: MonadPeer m => PWP -> m ()
handlePWP Unchoke = do
  peerData <- getPeerData
  updatePeerData $ peerData { peerChoking = False }
  requestNextChunk
handlePWP (Bitfield field) = handleBitfield field
handlePWP (Piece ix offset d) = receiveChunk (PieceId ix) offset d >> requestNextChunk
handlePWP (Have ix) = handleHave (PieceId ix)
handlePWP Interested = handleInterested
handlePWP (Request ix offset len) = do
  peerData <- getPeerData

  meta <- getMeta

  unless (amChoking peerData) $ do
    let defaultPieceLen = pieceLength $ info meta
    block <- readData (fromIntegral ix * fromIntegral defaultPieceLen + fromIntegral offset) (fromIntegral len)
    emit (Piece ix offset block)
handlePWP _ = return () -- logging?

-- | Marks the chunk as missing and marks as inactive chunk.
releaseActiveChunk :: MonadPeer m => PieceId -> ChunkId -> m ()
releaseActiveChunk piece@(PieceId pieceId) chunkId = do
  activeChunks <- getActiveChunks

  case Map.lookup (piece, chunkId) activeChunks of
    Just _ ->
      runMemory $ do
        dp <- getDownloadProgress piece
        case dp of
          Just cf ->
            let cf' = CF.markMissing cf chunkId
            in do setDownloadProgress piece cf'
                  modifyRequestablePieces (IntSet.insert $ fromIntegral pieceId)
          Nothing -> pure ()
    Nothing -> pure ()

  log $ \peerid -> logDebugN $ "Releasing " <> peerid <> " " <> T.pack (show (piece, chunkId))
  deregisterActiveChunk piece chunkId

-- | Runs the peer loop, processing all events.
entryPoint :: MonadPeer m => m ()
entryPoint = catchError processEvent onError
  where processEvent = forever (getPeerEvent >>= handler)
        handler (PWPEvent pwp) = handlePWP pwp
        handler (ErrorEvent err) = throwError err
        handler (SharedEvent RequestPiece) = requestNextChunk
        handler (SharedEvent Exit) = throwError ConnectionLost
        handler (SharedEvent Checkup) = do
          t <- getTime
          activeChunks <- getActiveChunks
          let timedOut = Map.filter (\date -> diffUTCTime t date > 10) activeChunks
              keys = fst <$> Map.toList timedOut
              allKeys = fst <$> Map.toList activeChunks
          log $ \peerid -> logDebugN $ "checkup for " <> peerid <> " " <> T.pack (show allKeys) <> " actually releasing " <> T.pack (show keys)
          traverse_ (uncurry releaseActiveChunk) keys

          when (Prelude.null keys) requestNextChunk
        onError ConnectionLost = do
          log $ \peerid -> logInfoN $ "ConnectionLost to " <> peerid
          cleanup
        onError (AssertionFailed what) = do
          log $ \peerid -> logErrorN $ "AssertionFailed for " <> peerid <> " with " <> T.pack what
          cleanup
        cleanup = do
          activeChunks <- getActiveChunks
          pData <- getPeerData
          updatePeerData $ pData { peerDataStopping = True }
          let keys = fst <$> Map.toList activeChunks
          log $ \peerid -> logInfoN $ "exiting from " <> peerid <> " and releasing " <> T.pack (show keys)
          traverse_ (uncurry releaseActiveChunk) keys

