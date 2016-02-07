{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

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
#ifdef TESTING
  PeerMonad(..)
, PeerEvent(..)
, handlePWP
#else
  PeerMonad()
#endif
, runPeerMonad
, PeerError(..)
, ActiveChunks

-- * Actions
, entryPoint
#ifdef TESTING
, requestNextChunk
, nextRequestOperation
, nextRequestOperationLinear
, nextRequestOperationLinearV2
, receiveChunk
, RequestOperation(..)
#endif

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
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Free.Church
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Strict
import Crypto.Hash.SHA1
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.ByteString as B
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Internal as BI
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word
import Data.Vector.Storable.Mutable as VS
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.FileWriter as FW
import Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.MemoryMonad
import Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Utility
import Network.BitTorrent.Types
import Prelude hiding (log)
import System.IO
import System.IO.Unsafe

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
type PeerMonadIO = ExceptT PeerError (ReaderT ClientState (StateT PeerState (LoggingT IO)))

-- | Encodes possible events that arrive in the 'PeerMonad'.
--
-- Retrieved by using 'getPeerEvent'.
data PeerEvent = PWPEvent PWP
               | SharedEvent SharedMessage
               | ErrorEvent PeerError
               deriving(Eq,Show)

-- | Encodes all possible operations on the PeerMonad.
-- Higher level operations use these to implement the protocol logic.
data PeerMonad a = forall b. RunMemory (F MemoryMonad b) (b -> a)
                | GetPeerData (PeerData -> a)
                | Emit PWP a
                | GetMeta (MetaInfo -> a)
                | ReadData Word32 Word32 (ByteString -> a)
                | WriteData Word32 ByteString a
                | UpdatePeerData PeerData a
                | GetPeerEvent (PeerEvent -> a)
                | RegisterActiveChunk PieceId ChunkId a
                | DeregisterActiveChunk PieceId ChunkId a
                | GetActiveChunks (ActiveChunks -> a)
                | GetTime (UTCTime -> a)
                | Throw PeerError a
                | Catch a (PeerError -> a)
                | Log (LoggingT IO ()) a

instance Functor PeerMonad where
  fmap f (RunMemory action next) = RunMemory action (fmap f next)
  fmap f (GetPeerData next) = GetPeerData (fmap f next)
  fmap f (Emit pwp next) = Emit pwp (f next)
  fmap f (GetMeta next) = GetMeta (fmap f next)
  fmap f (ReadData o l next) = ReadData o l (fmap f next)
  fmap f (WriteData o b next) = WriteData o b (f next)
  fmap f (UpdatePeerData pData next) = UpdatePeerData pData (f next)
  fmap f (GetPeerEvent next) = GetPeerEvent (fmap f next)
  fmap f (RegisterActiveChunk pieceId chunkId next) = RegisterActiveChunk pieceId chunkId (f next)
  fmap f (DeregisterActiveChunk pieceId chunkId next) = DeregisterActiveChunk pieceId chunkId (f next)
  fmap f (GetActiveChunks next) = GetActiveChunks (fmap f next)
  fmap f (GetTime next) = GetTime (fmap f next)
  fmap f (Throw err next) = Throw err (f next)
  fmap f (Catch action handler) = Catch (f action) (fmap f handler)
  fmap f (Log what next) = Log what (f next)

-- | Run a MemoryMonad expression as a single, atomic transaction.
runMemory :: F MemoryMonad a -> F PeerMonad a
runMemory stm = liftF $ RunMemory stm id
{-# INLINABLE runMemory #-}

-- | Get 'PeerData'.
getPeerData :: F PeerMonad PeerData
getPeerData = liftF $ GetPeerData id
{-# INLINABLE getPeerData #-}

-- | Send a message to the peer.
emit :: PWP -> F PeerMonad ()
emit pwp = liftF $ Emit pwp ()
{-# INLINABLE emit #-}

-- | Get 'MetaInfo'.
getMeta :: F PeerMonad MetaInfo
getMeta = liftF $ GetMeta id
{-# INLINABLE getMeta #-}

-- | Read data from disk.
readData :: Word32  -- ^ offset
         -> Word32  -- ^ length
         -> F PeerMonad ByteString
readData o l = liftF $ ReadData o l id
{-# INLINABLE readData #-}

-- | Write data to disk.
writeData :: Word32     -- ^ offset
          -> ByteString -- ^ data
          -> F PeerMonad ()
writeData o b = liftF $ WriteData o b ()
{-# INLINABLE writeData #-}

-- | Updates 'PeerData'.
-- Because the peer loop is the only owner, it's
-- safe to modify at any time and does not need to run in the 'MemoryMonad'.
updatePeerData :: PeerData -> F PeerMonad ()
updatePeerData pData = liftF $ UpdatePeerData pData ()
{-# INLINABLE updatePeerData #-}

-- | Blocks and waits on the next 'PeerEvent'.
getPeerEvent :: F PeerMonad PeerEvent
getPeerEvent = liftF $ GetPeerEvent id
{-# INLINABLE getPeerEvent #-}

-- | Registers an active chunk.
--
-- __NOTE:__ This is a temporary solution until storing
-- custom data is provided by 'PeerMonad'.
registerActiveChunk :: PieceId -> ChunkId -> F PeerMonad ()
registerActiveChunk pieceId chunkId = liftF $ RegisterActiveChunk pieceId chunkId ()
{-# INLINABLE registerActiveChunk #-}

-- | Removes an active chunk.
--
-- __NOTE:__ This is a temporary solution until storing
-- custom data is provided by 'PeerMonad'.
deregisterActiveChunk :: PieceId -> ChunkId -> F PeerMonad ()
deregisterActiveChunk pieceId chunkId = liftF $ DeregisterActiveChunk pieceId chunkId ()
{-# INLINABLE deregisterActiveChunk #-}

-- | Gets all remaining active chunks.
--
-- __NOTE:__ This is a temporary solution until storing
-- custom data is provided by 'PeerMonad'.
getActiveChunks :: F PeerMonad ActiveChunks
getActiveChunks = liftF $ GetActiveChunks id
{-# INLINABLE getActiveChunks #-}

-- | Get current time.
--
-- __NOTE:__ This is a temporary solution until a nicer interface
-- for timeouts & cancellables is implemented.
getTime :: F PeerMonad UTCTime
getTime = liftF $ GetTime id
{-# INLINABLE getTime #-}

-- | Throws an error.
throwError :: PeerError -> F PeerMonad ()
throwError err = liftF $ Throw err ()
{-# INLINABLE throwError #-}

-- | Catches errors for nested 'PeerMonad' expressions.
--
-- It works with asynchronous exceptions too, but masking
-- is not supported yet.
catchError :: F PeerMonad a -> (PeerError -> F PeerMonad a) -> F PeerMonad a
catchError action handler = join $ liftF $ Catch action handler
{-# INLINABLE catchError #-}

-- | Logs messages that are of 'MonadLogger' form.
log :: (T.Text -> LoggingT IO ()) -> F PeerMonad ()
log exp = do
  pData <- getPeerData
  case fromByteString (B64.encode (peerId pData)) of
    Just t -> liftF $ Log (exp t) ()
    Nothing -> return ()

-- | Runs a 'PeerMonad' in the IO monad.
--
-- Internally spawns threads to process events from multiple sources efficiently.
-- It forwards 'SharedMessage's and 'PWP' messages to the 'PeerMonad' to act upon.
--
-- Nested 'MemoryMonad' expressions are evaluated using STM.
runPeerMonad :: ClientState
             -> PeerData
             -> Handle -- ^ socket handle to communicate with the peer
             -> F PeerMonad a -- ^ PeerMonad expression to evaluate
             -> IO (Either PeerError a)
runPeerMonad state pData outHandle t = do
  privateChan <- newChan
  sharedChan <- dupChan (sharedMessages state)

  let peerState = PeerState pData privateChan outHandle Map.empty

  promise1 <- async $ messageForwarder outHandle privateChan
  promise2 <- async $ forever $ readChan sharedChan >>= writeChan privateChan . SharedEvent

  let action = runStderrLoggingT (evalStateT (runReaderT (runExceptT (inside t)) state) peerState)
  action `finally` (cancel promise1 *> cancel promise2 *> hClose outHandle)

  where inside = iterM evalPeerMonadIO
        messageForwarder handle privateChan = (do
          input <- BL.hGetContents handle
          let messages = messageStream input
          traverse_ (writeChan privateChan . PWPEvent) messages)
          `onException` writeChan privateChan (ErrorEvent ConnectionLost)
        messageStream :: BL.ByteString -> [PWP]
        messageStream input =
          case Binary.Get.runGetOrFail Binary.get input of
            Left _ -> []
            Right (rest, _, msg) -> msg : messageStream rest


evalPeerMonadIO :: PeerMonad (PeerMonadIO a) -> PeerMonadIO a
evalPeerMonadIO (RunMemory a next) = do
  state <- ask
  res <- liftIO $ atomically $ runMemoryMonadSTM state a
  next res
evalPeerMonadIO (GetPeerData next) =
  get >>= next . peerStateData
evalPeerMonadIO (Emit pwp next) = do
  state <- get
  let handle = peerStateHandle state

  res <- liftIO $ Catch.catchIOError
             (BL.hPut handle (Binary.encode pwp) *> return True)
             (const (return True))

  case res of
    True -> return ()
    False -> Except.throwError ConnectionLost

  next
evalPeerMonadIO (GetMeta next) = do
  meta <- metaInfo <$> ask
  next meta
evalPeerMonadIO (ReadData o l next) = do
  state <- ask
  let hdls = outputHandles state
      lock = outputLock state
  a <- liftIO $ FW.read hdls lock o l
  next a
evalPeerMonadIO (WriteData o d next) = do
  state <- ask
  let hdls = outputHandles state
      lock = outputLock state
  liftIO $ FW.write hdls lock o d
  next
evalPeerMonadIO (UpdatePeerData pData next) = do
  pState <- get
  put $ pState { peerStateData = pData }
  next
evalPeerMonadIO (GetPeerEvent next) = do
  pState <- get
  msg <- liftIO $ readChan $ peerStateChan pState
  next msg
evalPeerMonadIO (RegisterActiveChunk pieceId chunkId next) = do
  pState <- get

  t <- liftIO getCurrentTime
  put $ pState { peerStateActiveChunks =
                 Map.insert (pieceId, chunkId)
                            t
                            (peerStateActiveChunks pState) }
  next
evalPeerMonadIO (DeregisterActiveChunk pieceId chunkId next) = do
  pState <- get
  let activeChunks = peerStateActiveChunks pState

  put $ pState { peerStateActiveChunks = Map.delete (pieceId, chunkId) activeChunks }

  next
evalPeerMonadIO (GetActiveChunks next) = do
  pState <- get
  next (peerStateActiveChunks pState)
evalPeerMonadIO (GetTime next) = liftIO getCurrentTime >>= next
evalPeerMonadIO (Throw e next) = Except.throwError e *> next
evalPeerMonadIO (Catch action handler) =
  Except.catchError action handler
evalPeerMonadIO (Log exp next) = lift (lift (lift exp)) *> next

receiveChunk :: PieceId -> Word32 -> ByteString -> F PeerMonad ()
receiveChunk piece offset d = do
  let chunkIndex = ChunkId (divideSize offset defaultChunkSize)

  wasMarked <- runMemory $ {-# SCC "receiveChunk--memory" #-} do
    chunks <- getChunks
    case Map.lookup piece chunks of
      Just chunkField -> do
        let chunkField' = CF.markCompleted chunkField chunkIndex
        modifyChunks $ Map.insert piece chunkField'
        return True
      _ -> return False -- someone already filled this

  deregisterActiveChunk piece chunkIndex
  pData <- getPeerData
  updatePeerData (pData { requestsLive = requestsLive pData - 1 })

  if wasMarked
    then do
      meta <- getMeta
      let infoDict = info meta
          defaultPieceLen = pieceLength infoDict
          PieceId pix = piece
      writeData (pix * defaultPieceLen + offset) d
      processPiece piece
    else return ()

processPiece :: PieceId -> F PeerMonad ()
processPiece piece@(PieceId pieceId) = do
  meta <- getMeta
  let infoDict = info meta
      defaultPieceLen = pieceLength infoDict
      totalSize = sum (Meta.length <$> Meta.files infoDict)
      pieceSize = expectedPieceSize totalSize defaultPieceLen piece

  isCompleted <- runMemory $ do
    cf <- fmap (Map.lookup piece) getChunks
    return $ fmap CF.isCompleted cf

  when (isCompleted == Just True) $ do
    d <- readData (pieceId * defaultPieceLen) pieceSize

    dataToWrite <- runMemory $ do
      chunks <-  getChunks
      case Map.lookup piece chunks of
        Just chunkField -> do
          let getPieceHash (PieceId n) =
                B.take 20 $ B.drop (fromIntegral n * 20) $ pieces infoDict

          case CF.isCompleted chunkField of
            True -> {-# SCC "processPiece--onComplete" #-} do
              {-# SCC "modifyChunks" #-} modifyChunks $ Map.delete piece
              if ({-# SCC "hash-data" #-} hash d) == getPieceHash piece
                then do
                  bitfield <- getBitfield
                  let newBitfield = {-# SCC "newBitField" #-} BF.set bitfield pieceId True
                  {-# SCC "modifyChunks" #-} modifyBitfield (const newBitfield)
                  {-# SCC "modifyAvailability" #-} modifyAvailability (PS.addToAvailability newBitfield .
                                      PS.removeFromAvailability bitfield)
                  {-# SCC "modifyRequestablePieces" #-} modifyRequestablePieces (`IntSet.difference` IntSet.singleton (fromIntegral pieceId))
                  return (Just d)
                else return Nothing -- we remove it from Chunks
                                    -- but do not modify the bitfield,
                                    -- it will be reacquired again
            False -> return Nothing
        Nothing -> return Nothing

    case dataToWrite of
      Just d -> {-# SCC "processPiece--writeData" #-} writeData (defaultPieceLen * pieceId) d
      Nothing -> return ()

handleBitfield :: ByteString -> F PeerMonad ()
handleBitfield field = do
  peerData <- getPeerData
  newBitField <- runMemory $ do
    len <- BF.length <$> getBitfield
    let newBitField = BF.BitField field len
    modifyAvailability $ PS.addToAvailability newBitField
    return newBitField
  updatePeerData $ peerData { peerBitField = newBitField }
  emit Interested

handleHave :: PieceId -> F PeerMonad ()
handleHave (PieceId ix) = do
  peerData <- getPeerData
  let oldBf = peerBitField peerData
      newBf = BF.set oldBf ix True
      peerData' = peerData { peerBitField = newBf }
  runMemory $
    modifyAvailability $ PS.addToAvailability newBf . PS.removeFromAvailability oldBf
  updatePeerData peerData'

handleInterested :: F PeerMonad ()
handleInterested = do
  peerData <- getPeerData
  when (amChoking peerData) $ do
    emit Unchoke
    updatePeerData $ peerData { amChoking = False, peerInterested = True }

data RequestOperation = RequestChunk PieceId ChunkId PWP
                      | Raise PeerError

claimChunk :: Word32
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
      modifyChunks $ Map.insert piece chunkField'
      return $ Just $ RequestChunk piece chunk request
    _ -> return Nothing
  where nextChunkSize :: PieceId -> ChunkId -> Word32
        nextChunkSize =
          expectedChunkSize totalSize pieceLen defaultChunkSize

nextRequestOperation :: PeerData -> MetaInfo -> F MemoryMonad (Maybe RequestOperation)
nextRequestOperation peerData meta = do
  chunks <- getChunks
  avData <- getAvailability
  ourBitField <- getBitfield

  let peersBitField = peerBitField peerData
      infoDict = info meta
      defaultPieceLen = pieceLength infoDict
      totalSize = {-# SCC "totalSize" #-} force $ sum (Meta.length <$> Meta.files infoDict)

      requestedBitField = {-# SCC "requestedBitField" #-} force $ BF.fromChunkFields (BF.length peersBitField)
                                             (Map.toList chunks)
      completedBitField = {-# SCC "completedBitField" #-} force $ BF.union ourBitField requestedBitField
      pendingBitField = {-# SCC "pendingBitField" #-} force $ BF.negate $ BF.difference peersBitField completedBitField
      -- logic for the bitfield operations is:
      -- still_needed = peer's - (ours + fully_requested)
      -- negation is only for getNextPiece, but that should be
      -- changed in the future

  case PS.getNextPiece pendingBitField avData of
    Nothing -> return Nothing
    Just piece@(PieceId pieceId) ->
      let pieceLen = expectedPieceSize totalSize defaultPieceLen piece
      in case () of
        _ | not (BF.get peersBitField pieceId) ->
          return (Just $ Raise $ AssertionFailed ("Peer does not have it, peer:" ++ show (BF.get peersBitField pieceId) ++ ", us:" ++ show (BF.get ourBitField pieceId) ++ ", bfrequested:" ++ show (BF.get requestedBitField pieceId)))
        _ | BF.get ourBitField pieceId ->
          return (Just $ Raise $ AssertionFailed "We already have it")
        _ ->
          case Map.lookup piece chunks of
            Nothing -> do
              let chunksCount = chunksInPiece pieceLen defaultChunkSize
                  chunkField = CF.newChunkField (fromIntegral chunksCount)
              claimChunk totalSize pieceLen chunkField piece
            Just chunkInfo -> claimChunk totalSize pieceLen chunkInfo piece

nextRequestOperationLinear :: PeerData -> MetaInfo -> F MemoryMonad (Maybe RequestOperation)
nextRequestOperationLinear peerData meta = do
  chunks <- getChunks
  ourBitField <- getBitfield

  let peersBitField = peerBitField peerData
      infoDict = info meta
      defaultPieceLen = pieceLength infoDict
      totalSize = {-# SCC "totalSize" #-} force $ sum (Meta.length <$> Meta.files infoDict)
      findPiece n | n == (BF.length ourBitField) = Nothing
      findPiece n = case BF.get ourBitField n of
        False -> case Map.lookup (PieceId n) chunks of
          Nothing -> Just (PieceId n)
          Just chunkField -> case CF.getNextChunk chunkField of
            Just _ -> Just (PieceId n)
            _ -> findPiece (n + 1)
        True -> findPiece (n + 1)
      nextPiece = findPiece 0

  case nextPiece of
    Nothing -> return Nothing
    Just piece@(PieceId pieceId) ->
      let pieceLen = expectedPieceSize totalSize defaultPieceLen piece
      in case () of
        _ | not (BF.get peersBitField pieceId) ->
          return (Just $ Raise $ AssertionFailed ("Peer does not have it, peer:" ++ show (BF.get peersBitField pieceId) ++ ", us:" ++ show (BF.get ourBitField pieceId)))
        _ ->
          case Map.lookup piece chunks of
            Nothing -> do
              let chunksCount = chunksInPiece pieceLen defaultChunkSize
                  chunkField = CF.newChunkField (fromIntegral chunksCount)
              claimChunk totalSize pieceLen chunkField piece
            Just chunkInfo -> claimChunk totalSize pieceLen chunkInfo piece

nextRequestOperationLinearV2 :: PeerData -> MetaInfo -> F MemoryMonad (Maybe RequestOperation)
nextRequestOperationLinearV2 peerData meta = do
  chunks <- getChunks
  ourBitField <- getBitfield
  requestablePieces <- getRequestablePieces

  let peersBitField = peerBitField peerData
      infoDict = info meta
      defaultPieceLen = pieceLength infoDict
      totalSize = {-# SCC "totalSize" #-} force $ sum (Meta.length <$> Meta.files infoDict)
      findPiece n | n == (BF.length ourBitField) = Nothing
      findPiece n = case IntSet.lookupGE (fromIntegral n) requestablePieces of
        Just k ->
          let piece = PieceId (fromIntegral k)
              next = findPiece (fromIntegral k + 1)
          in case Map.lookup piece chunks of
            Nothing -> Just piece
            Just chunkField -> case CF.getNextChunk chunkField of
              Just _ -> Just piece
              _ -> next
            _ -> next
        _ -> Nothing
      nextPiece = findPiece 0

  case nextPiece of
    Nothing -> return Nothing
    Just piece@(PieceId pieceId) ->
      let pieceLen = expectedPieceSize totalSize defaultPieceLen piece
      in case () of
        _ | not (BF.get peersBitField pieceId) ->
          return (Just $ Raise $ AssertionFailed ("Peer does not have it, peer:" ++ show (BF.get peersBitField pieceId) ++ ", us:" ++ show (BF.get ourBitField pieceId)))
        _ ->
          case Map.lookup piece chunks of
            Nothing -> do
              let chunksCount = chunksInPiece pieceLen defaultChunkSize
                  chunkField = CF.newChunkField (fromIntegral chunksCount)
              claimChunk totalSize pieceLen chunkField piece
            Just chunkInfo -> claimChunk totalSize pieceLen chunkInfo piece

requestNextChunk :: F PeerMonad ()
requestNextChunk = do
  peerData <- getPeerData
  meta <- getMeta
  unless (peerDataStopping peerData) $
    when (requestsLive peerData < maxRequestsPerPeer) $
    unless (peerChoking peerData) $ do
      operation <- runMemory $ nextRequestOperationLinearV2 peerData meta

      case operation of
        Just (RequestChunk pieceId chunkId pwpRequest) -> do
          registerActiveChunk pieceId chunkId
          let modifiedPeer = peerData { requestsLive = requestsLive peerData + 1 }
          updatePeerData modifiedPeer
          emit pwpRequest
          requestNextChunk
        Just (Raise err) -> throwError err
        Nothing -> return ()

handlePWP :: PWP -> F PeerMonad ()
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
    block <- readData (ix * defaultPieceLen + offset) len
    emit (Piece ix offset block)
handlePWP _ = return () -- logging?

-- | Marks the chunk as missing and marks as inactive chunk.
releaseActiveChunk :: PieceId -> ChunkId -> F PeerMonad ()
releaseActiveChunk pieceId chunkId = do
  activeChunks <- getActiveChunks

  case Map.lookup (pieceId, chunkId) activeChunks of
    Just _ ->
      runMemory $ modifyChunks $ \chunks ->
        case Map.lookup pieceId chunks of
          Just cf ->
            let cf' = CF.markMissing cf chunkId
            in Map.insert pieceId cf' chunks
          Nothing -> chunks
    Nothing -> pure ()

  deregisterActiveChunk pieceId chunkId

-- | Runs the peer loop, processing all events.
entryPoint :: F PeerMonad ()
entryPoint = catchError processEvent onError
  where processEvent = forever (getPeerEvent >>= handler)
        handler (PWPEvent pwp) = handlePWP pwp
        handler (ErrorEvent err) = throwError err
        handler (SharedEvent RequestPiece) = requestNextChunk
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
          log $ \peerid -> logDebugN $ "ConnectionLost to " <> peerid
          cleanup
        onError (AssertionFailed what) = do
          log $ \peerid -> logDebugN $ "AssertionFailed for " <> peerid <> " with " <> T.pack what
          cleanup
        cleanup = do
          activeChunks <- getActiveChunks
          pData <- getPeerData
          updatePeerData $ pData { peerDataStopping = True }
          let keys = fst <$> Map.toList activeChunks
          log $ const $ logDebugN "kek"
          log $ \peerid -> logDebugN $ "exiting from " <> peerid <> " and releasing " <> T.pack (show keys)
          traverse_ (uncurry releaseActiveChunk) keys

