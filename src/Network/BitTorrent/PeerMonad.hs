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
, Cleanups

-- * Actions
, entryPoint
#ifdef TESTING
, requestNextChunk
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
, registerCleanup
, deregisterCleanup
, releaseCleanup
, getCleanups
, getTime
, catchError
, throwError
) where

import Control.Concurrent
import Control.Concurrent.Async
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
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Internal as BI
import Data.Foldable (traverse_)
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
type Cleanups = Map (Word32, Word32) UTCTime

data PeerState = PeerState { peerStateData :: !PeerData
                           , peerStateChan :: Chan PeerEvent
                           , peerStateHandle :: Handle
                           , peerStateCleanups :: Cleanups
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
                | RegisterCleanup Word32 Word32 a
                | DeregisterCleanup Word32 Word32 a
                | GetCleanups (Cleanups -> a)
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
  fmap f (RegisterCleanup pieceId chunkId next) = RegisterCleanup pieceId chunkId (f next)
  fmap f (DeregisterCleanup pieceId chunkId next) = DeregisterCleanup pieceId chunkId (f next)
  fmap f (GetCleanups next) = GetCleanups (fmap f next)
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
writeData :: Word32 -- ^ offset
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

-- | Registers a cleanup.
--
-- __NOTE:__ This is a temporary solution until storing
-- custom data is provided by 'PeerMonad'.
registerCleanup :: Word32 -> Word32 -> F PeerMonad ()
registerCleanup pieceId chunkId = liftF $ RegisterCleanup pieceId chunkId ()
{-# INLINABLE registerCleanup #-}

-- | Removes a cleanup.
--
-- __NOTE:__ This is a temporary solution until storing
-- custom data is provided by 'PeerMonad'.
deregisterCleanup :: Word32 -> Word32 -> F PeerMonad ()
deregisterCleanup pieceId chunkId = liftF $ DeregisterCleanup pieceId chunkId ()
{-# INLINABLE deregisterCleanup #-}

-- | Gets all remaining cleanups.
--
-- __NOTE:__ This is a temporary solution until storing
-- custom data is provided by 'PeerMonad'.
getCleanups :: F PeerMonad Cleanups
getCleanups = liftF $ GetCleanups id
{-# INLINABLE getCleanups #-}

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
  let hdl = outputHandle state
      lock = outputLock state
  a <- liftIO $ FW.read hdl lock o l
  next a
evalPeerMonadIO (WriteData o d next) = do
  state <- ask
  let hdl = outputHandle state
      lock = outputLock state
  liftIO $ FW.write hdl lock o d
  next
evalPeerMonadIO (UpdatePeerData pData next) = do
  pState <- get
  put $ pState { peerStateData = pData }
  next
evalPeerMonadIO (GetPeerEvent next) = do
  pState <- get
  msg <- liftIO $ readChan $ peerStateChan pState
  next msg
evalPeerMonadIO (RegisterCleanup pieceId chunkId next) = do
  pState <- get

  t <- liftIO getCurrentTime
  put $ pState { peerStateCleanups =
                 Map.insert (pieceId, chunkId)
                            t
                            (peerStateCleanups pState) }
  next
evalPeerMonadIO (DeregisterCleanup pieceId chunkId next) = do
  pState <- get
  let cleanups = peerStateCleanups pState

  put $ pState { peerStateCleanups = Map.delete (pieceId, chunkId) cleanups }

  next
evalPeerMonadIO (GetCleanups next) = do
  pState <- get
  next (peerStateCleanups pState)
evalPeerMonadIO (GetTime next) = liftIO getCurrentTime >>= next
evalPeerMonadIO (Throw e next) = Except.throwError e *> next
evalPeerMonadIO (Catch action handler) =
  Except.catchError action handler
evalPeerMonadIO (Log exp next) = lift (lift (lift exp)) *> next

receiveChunk :: Word32 -> Word32 -> ByteString -> F PeerMonad ()
receiveChunk ix offset d = do
  let chunkIndex = divideSize offset defaultChunkSize

  chunkField <- runMemory $ do
    chunks <- getChunks
    case Map.lookup ix chunks of
      Just (chunkField, chunkData) -> do
        do
          -- copy the incoming data into appropriate place in chunkData
          let (ptr, o, len) = BI.toForeignPtr chunkData
              chunkVector = VS.unsafeFromForeignPtr ptr o len
              (ptr', o', len') = BI.toForeignPtr d
              dataVector = VS.unsafeFromForeignPtr ptr' o' len'
              dest = VS.take (B.length d) $ VS.drop (fromIntegral offset) chunkVector
              src = dataVector
          unsafePerformIO $ VS.copy dest src >> return (return ())
        let chunkField' = CF.markCompleted chunkField (fromIntegral chunkIndex)

        modifyChunks $ Map.insert ix (chunkField', chunkData)
        return True
      _ -> return False -- someone already filled this

  deregisterCleanup ix chunkIndex
  pData <- getPeerData
  updatePeerData (pData { requestsLive = requestsLive pData - 1 })
  when chunkField $ processPiece ix

processPiece :: Word32 -> F PeerMonad ()
processPiece ix = do
  meta <- getMeta
  let infoDict = info meta
      pieces' = pieces infoDict
      defaultPieceLen = pieceLength $ info meta
      getPieceHash n = B.take 20 $ B.drop (fromIntegral n * 20) pieces'

  dataToWrite <- runMemory $ do
    Just (chunkField, d) <- Map.lookup ix <$> getChunks
    let hashCheck = hash d == getPieceHash ix

    case CF.isCompleted chunkField of
      True -> do
        modifyChunks $ Map.delete ix
        if hashCheck
          then do
            bf <- getBitfield
            if BF.get bf ix
              then return Nothing
              else modifyBitfield (\bf' -> BF.set bf' ix True) *> return (Just d)
          else return Nothing
               -- because we remove the entry from (pieceChunks state),
               -- but not indicate it as downloaded in the bitField,
               -- it will be reacquired again
      False -> return Nothing

  case dataToWrite :: Maybe ByteString of
    Just d -> writeData (defaultPieceLen * ix) d
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

handleHave :: Word32 -> F PeerMonad ()
handleHave ix = do
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

data RequestOperation = RequestChunk Word32 Word32 PWP
                      | Raise PeerError

requestNextChunk :: F PeerMonad ()
requestNextChunk = do
  peerData <- getPeerData
  when (not $ peerDataStopping peerData) $
    when (requestsLive peerData < maxRequestsPerPeer) $
      unless (peerChoking peerData) $ do
        meta <- getMeta
        (operation, shouldRunAgain) <- runMemory $ do
          chunks <- getChunks
          avData <- getAvailability
          bf <- getBitfield

          let pbf = peerBitField peerData
              infoDict = info meta
              defaultPieceLen :: Word32
              defaultPieceLen = pieceLength infoDict
              totalSize = Meta.length infoDict
              bfrequested = BF.fromChunkFields
                              (BF.length pbf)
                              (Map.toList (fst <$> chunks))
              bfdone = BF.union bf bfrequested
              bfrequestable = BF.negate $ BF.difference pbf bfdone
              -- logic for the bitfield operations is:
              -- still_needed = peer's - (ours + fully_requested)
              -- negation is only for getNextPiece, but that should be
              -- changed in the future

          case PS.getNextPiece bfrequestable avData of
            Nothing -> return (Nothing, False)
            Just ix -> do
              case () of
                _ | not (BF.get pbf ix) ->
                  return (Just $ Raise $ AssertionFailed ("Peer does not have it, peer:" ++ show (BF.get pbf ix) ++ ", us:" ++ show (BF.get bf ix) ++ ", bfrequested:" ++ show (BF.get bfrequested ix)), False)
                _ | BF.get bf ix ->
                  return (Just $ Raise $ AssertionFailed "We already have it", False)
                _ -> do
                  let pieceLen = expectedPieceSize totalSize ix defaultPieceLen
                  case Map.lookup ix chunks of
                    Just (chunkField, chunkData) -> do
                      let nextChunk =  CF.getNextChunk chunkField
                      case nextChunk of
                        Just (chunkField', cix) -> do
                          let nextChunkSize = expectedChunkSize totalSize ix (fromIntegral cix+1) pieceLen defaultChunkSize
                              request = Request ix (fromIntegral cix * defaultChunkSize) nextChunkSize
                          modifyChunks (Map.insert ix (chunkField', chunkData))
                          return (Just (RequestChunk ix (fromIntegral cix) request), True)
                        _ -> return (Nothing, False)
                    Nothing -> do
                      let chunksCount = chunksInPieces pieceLen defaultChunkSize
                          chunkData = B.replicate (fromIntegral pieceLen) 0
                          insertion = (CF.newChunkField (fromIntegral chunksCount), chunkData)
                      modifyChunks $ Map.insert ix insertion
                      return (Nothing, True)

        case operation of
          Just (RequestChunk pid cid request) -> do
            registerCleanup pid cid
            let modifiedPeer = peerData { requestsLive = requestsLive peerData + 1 }
            updatePeerData modifiedPeer
            emit request
          Just (Raise err) -> throwError err
          Nothing -> return ()

        when shouldRunAgain requestNextChunk

handlePWP :: PWP -> F PeerMonad ()
handlePWP Unchoke = do
  peerData <- getPeerData
  updatePeerData $ peerData { peerChoking = False }
  requestNextChunk
handlePWP (Bitfield field) = handleBitfield field
handlePWP (Piece ix offset d) = receiveChunk ix offset d >> requestNextChunk
handlePWP (Have ix) = handleHave ix
handlePWP Interested = handleInterested
handlePWP (Request ix offset len) = do
  peerData <- getPeerData
  meta <- getMeta

  unless (amChoking peerData) $ do
    let defaultPieceLen = pieceLength $ info meta
    block <- readData (ix * defaultPieceLen + offset) len
    emit (Piece ix offset block)
handlePWP _ = return () -- logging?

-- | Marks the chunk as missing and deregistering the cleanup.
releaseCleanup :: Word32 -> Word32 -> F PeerMonad ()
releaseCleanup pieceId chunkId = do
  cleanups <- getCleanups

  case Map.lookup (pieceId, chunkId) cleanups of
    Just _ ->
      runMemory $ modifyChunks $ \chunks ->
        case Map.lookup pieceId chunks of
          Just (cf, d) ->
            let cf' = CF.markMissing cf (fromIntegral chunkId)
            in Map.insert pieceId (cf', d) chunks
          Nothing -> chunks
    Nothing -> pure ()

  deregisterCleanup pieceId chunkId

-- | Runs the peer loop, processing all events.
entryPoint :: F PeerMonad ()
entryPoint = catchError processEvent onError
  where processEvent = forever (getPeerEvent >>= handler)
        handler (PWPEvent pwp) = handlePWP pwp
        handler (ErrorEvent err) = throwError err
        handler (SharedEvent RequestPiece) = requestNextChunk
        handler (SharedEvent Checkup) = do
          t <- getTime
          cleanups <- getCleanups
          let timedOut = Map.filter (\date -> diffUTCTime t date > 10) cleanups
              keys = fst <$> Map.toList timedOut
              allKeys = fst <$> Map.toList cleanups
          pData <- getPeerData
          log $ \peerid -> logDebugN $ "checkup for " <> peerid <> " " <> T.pack (show allKeys) <> " actually releasing " <> T.pack (show keys)
          traverse_ (uncurry releaseCleanup) keys

          when (Prelude.null keys) requestNextChunk
        onError ConnectionLost = do
          pData <- getPeerData
          log $ \peerid -> logDebugN $ "ConnectionLost to " <> peerid
          cleanup
        onError (AssertionFailed what) = do
          pData <- getPeerData
          log $ \peerid -> logDebugN $ "AssertionFailed for " <> peerid <> " with " <> T.pack what
          cleanup
        cleanup = do
          cleanups <- getCleanups
          pData <- getPeerData
          updatePeerData $ pData { peerDataStopping = True }
          let keys = fst <$> Map.toList cleanups
          log $ const $ logDebugN "kek"
          log $ \peerid -> logDebugN $ "exiting from " <> peerid <> " and releasing " <> T.pack (show keys)
          traverse_ (uncurry releaseCleanup) keys

