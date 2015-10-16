{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Network.BitTorrent.PeerMonad (
  runPeerMonad
, entryPoint

-- for testing, temporarily
, handlePWP
, getPeerData
, getPeerEvent
, PeerEvent(..)
, updatePeerData
, registerCleanup
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception.Base
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource
import Crypto.Hash.SHA1
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal as BI
import Data.Foldable (traverse_)
import Data.Map.Strict as Map
import Data.Word
import Data.Vector.Storable.Mutable as VS
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.FileWriter as FW
import Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.PeerSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Utility
import Network.BitTorrent.Types
import System.IO
import System.IO.Unsafe
import System.Random hiding(next)

data PeerState = PeerState { peerStateData :: PeerData
                           , peerStateChan :: Chan PeerEvent
                           , peerStateHandle :: Handle
                           , peerStateCleanups :: Map (Word32, Word32) ReleaseKey
                           }

-- TODO Logger
type PeerMonadIO = ResourceT (ReaderT ClientState (StateT PeerState IO))

type Chunks = Map Word32 (ChunkField, ByteString)

data MemoryMonad a = GetChunks (Chunks -> a)
                   | ModifyChunks (Chunks -> Chunks) a
                   | ReadBitfield (BF.BitField -> a)
                   | ModifyBitfield (BF.BitField -> BF.BitField) a
                   | ReadAvailability (AvailabilityData -> a)
                   | ModifyAvailability (AvailabilityData -> AvailabilityData) a
                   deriving(Functor)

getChunks :: F MemoryMonad Chunks
getChunks = liftF $ GetChunks id
{-# INLINABLE getChunks #-}

modifyChunks :: (Chunks -> Chunks) -> F MemoryMonad ()
modifyChunks mut = liftF $ ModifyChunks mut ()
{-# INLINABLE modifyChunks #-}

getBitfield :: F MemoryMonad BF.BitField
getBitfield = liftF $ ReadBitfield id
{-# INLINABLE getBitfield #-}

modifyBitfield :: (BF.BitField -> BF.BitField) -> F MemoryMonad ()
modifyBitfield mut = liftF $ ModifyBitfield mut ()
{-# INLINABLE modifyBitfield #-}

getAvailability :: F MemoryMonad AvailabilityData
getAvailability = liftF $ ReadAvailability id
{-# INLINABLE getAvailability #-}

modifyAvailability :: (AvailabilityData -> AvailabilityData) -> F MemoryMonad ()
modifyAvailability mut = liftF $ ModifyAvailability mut ()
{-# INLINABLE modifyAvailability #-}

evalMemoryMonad :: ClientState -> MemoryMonad (STM a) -> STM a
evalMemoryMonad state (GetChunks next) = do
  chunks <- readTVar (pieceChunks state)
  next chunks
evalMemoryMonad state (ModifyChunks f next) = do
  modifyTVar' (pieceChunks state) f
  next
evalMemoryMonad state (ReadBitfield next) = do
  res <- readTVar (bitField state)
  next res
evalMemoryMonad state (ModifyBitfield mut next) = do
  modifyTVar' (bitField state) mut
  next
evalMemoryMonad state (ReadAvailability next) = do
  res <- readTVar (availabilityData state)
  next res
evalMemoryMonad state (ModifyAvailability mut next) = do
  modifyTVar' (availabilityData state) mut
  next

runMemoryMonad :: ClientState -> F MemoryMonad a -> STM a
runMemoryMonad state = iterM (evalMemoryMonad state)

data PeerEvent = PWPEvent PWP | SharedEvent SharedMessage

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

peerUnchoked :: F PeerMonad ()
peerUnchoked = do
  peerData <- getPeerData
  updatePeerData $ peerData { peerChoking = False }
{-# INLINABLE peerUnchoked #-}

runMemory :: F MemoryMonad a -> F PeerMonad a
runMemory stm = liftF $ RunMemory stm id
{-# INLINABLE runMemory #-}

getPeerData :: F PeerMonad PeerData
getPeerData = liftF $ GetPeerData id
{-# INLINABLE getPeerData #-}

emit :: PWP -> F PeerMonad ()
emit pwp = liftF $ Emit pwp ()
{-# INLINABLE emit #-}

getMeta :: F PeerMonad MetaInfo
getMeta = liftF $ GetMeta id
{-# INLINABLE getMeta #-}

readData :: Word32 -> Word32 -> F PeerMonad ByteString
readData o l = liftF $ ReadData o l id
{-# INLINABLE readData #-}

writeData :: Word32 -> ByteString -> F PeerMonad ()
writeData o b = liftF $ WriteData o b ()
{-# INLINABLE writeData #-}

updatePeerData :: PeerData -> F PeerMonad ()
updatePeerData pData = liftF $ UpdatePeerData pData ()

getPeerEvent :: F PeerMonad PeerEvent
getPeerEvent = liftF $ GetPeerEvent id

registerCleanup :: Word32 -> Word32 -> F PeerMonad ()
registerCleanup pieceId chunkId = liftF $ RegisterCleanup pieceId chunkId ()

deregisterCleanup :: Word32 -> Word32 -> F PeerMonad ()
deregisterCleanup pieceId chunkId = liftF $ DeregisterCleanup pieceId chunkId ()

runPeerMonad :: ClientState -> PeerData -> Handle -> F PeerMonad a -> IO a
runPeerMonad state pData outHandle t = do
  pwpChan <- newChan
  sharedChan <- dupChan (sharedMessages state)

  let peerState = PeerState pData pwpChan outHandle Map.empty

  void $ forkIO $ messageForwarder outHandle pwpChan
  void $ forkIO $ forever $ readChan sharedChan >>= writeChan pwpChan . SharedEvent

  -- TODO: store this threadId for killing later
  evalStateT (runReaderT (runResourceT (inside t)) state) peerState
  where inside = iterM evalPeerMonadIO
        messageForwarder handle pwpChan = (do
          input <- BL.hGetContents handle
          let messages = messageStream input
          traverse_ (writeChan pwpChan . PWPEvent) messages)
          `onException` writeChan pwpChan (PWPEvent undefined) -- to crash & release
        messageStream :: BL.ByteString -> [PWP]
        messageStream input =
          case Binary.Get.runGetOrFail Binary.get input of
            Left _ -> []
            Right (rest, _, msg) -> msg : messageStream rest


evalPeerMonadIO :: PeerMonad (PeerMonadIO a) -> PeerMonadIO a
evalPeerMonadIO (RunMemory a next) = do
  state <- ask
  res <- liftIO $ atomically $ runMemoryMonad state a
  next res
evalPeerMonadIO (GetPeerData next) = do
  PeerState pData _ _ _ <- get
  next pData
evalPeerMonadIO (Emit pwp next) = do
  PeerState _ _ handle _ <- get
  liftIO $ BL.hPut handle (Binary.encode pwp)
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
  state <- ask
  pState <- get
  let release _ = do
        branch <- atomically $ do
          chunks <- readTVar (pieceChunks state)
          case Map.lookup pieceId chunks of
            Just (cf, d) -> do
              let cf' = CF.markMissing cf chunkId
              modifyTVar' (pieceChunks state) (Map.insert pieceId (cf', d))
              pure 1
            Nothing -> pure 0
        writeChan (sharedMessages state) WakeUp

  (releaseKey, _) <- allocate (pure ()) release
  put $ pState { peerStateCleanups = Map.insert (pieceId, chunkId) releaseKey (peerStateCleanups pState) }
  next
evalPeerMonadIO (DeregisterCleanup pieceId chunkId next) = do
  state <- ask
  pState <- get
  let cleanups = peerStateCleanups pState

  case Map.lookup (pieceId, chunkId) cleanups of
    Just releaseKey -> do
      void $ unprotect releaseKey
      put $ pState { peerStateCleanups = Map.delete (pieceId, chunkId) cleanups }
    Nothing -> pure ()

  next

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
        let chunkField' = CF.markCompleted chunkField chunkIndex

        modifyChunks $ Map.insert ix (chunkField', chunkData)
        return True
      _ -> return False -- someone already filled this

  deregisterCleanup ix chunkIndex
  pData <- getPeerData
  updatePeerData (pData { requestsLive = requestsLive pData - 1 })
  when chunkField $ processPiece ix

processPiece :: Word32 -> F PeerMonad ()
processPiece ix = do
  Just (chunkField, d) <- runMemory (Map.lookup ix <$> getChunks)
  when (CF.isCompleted chunkField) $ do
    meta <- getMeta
    let infoDict = info $ meta
        pieces' = pieces infoDict
        defaultPieceLen = pieceLength $ info $ meta
        getPieceHash n = B.take 20 $ B.drop (fromIntegral n * 20) pieces'
        hashCheck = hash d == getPieceHash ix

    {-unless hashCheck $ do
      print $ "Validating hashes " <> show hashCheck
      print ix-}

    wasSetAlready <- runMemory $ do
      modifyChunks $ Map.delete ix
      if hashCheck
        then do
          bf <- getBitfield
          let wasSetAlready = BF.get bf ix
          unless wasSetAlready $
            modifyBitfield (\bf' -> BF.set bf' ix True)
          return wasSetAlready
        else return False
      -- because we remove the entry from (pieceChunks state),
      -- but not indicate it as downloaded in the bitField,
      -- it will be reacquired again

    when (hashCheck && not wasSetAlready) $
      writeData (defaultPieceLen * ix) d

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
  runMemory $ do
    modifyAvailability $ PS.addToAvailability newBf . PS.removeFromAvailability oldBf
  updatePeerData peerData'

handleInterested :: F PeerMonad ()
handleInterested = do
  peerData <- getPeerData
  when (amChoking peerData) $ do
    emit Unchoke
    updatePeerData $ peerData { amChoking = False, peerInterested = True }

requestNextPiece :: F PeerMonad ()
requestNextPiece = do
  peerData <- getPeerData
  unless (peerChoking peerData) $ do
    (chunks, bf, avData) <- runMemory $ do
      chunks <- getChunks
      avData <- getAvailability
      bf <- getBitfield
      return (chunks, bf, avData)
    meta <- getMeta
    let pbf = peerBitField peerData
        infoDict = info meta
        defaultPieceLen :: Word32
        defaultPieceLen = pieceLength infoDict
        totalSize = Meta.length infoDict

        lastStage = False -- BF.completed bf > 0.9 -- as long as the unsafe buffer copies happen
        bfrequestable = if lastStage
                          then bf
                          else Map.foldlWithKey' (\bit ix (cf, _) ->
                                 if not (BF.get pbf ix) || CF.isRequested cf
                                   then BF.set bit ix True
                                   else bit) bf chunks

        incompletePieces = PS.getIncompletePieces bfrequestable

    nextPiece <- if lastStage
                   then do
                     if Prelude.length incompletePieces - 1 > 0
                       then return $ unsafePerformIO $ do
                         rand <- randomRIO (0, Prelude.length incompletePieces - 1)
                         return $ Just $ incompletePieces !! rand
                       else return Nothing
                   else return $ PS.getNextPiece bfrequestable avData

    case nextPiece of
      Nothing -> return () -- putStrLn "we have all dem pieces"
      Just ix -> do
        let pieceLen = expectedPieceSize totalSize ix defaultPieceLen
        case Map.lookup ix chunks of
          Just (chunkField, chunkData) -> do
            let Just (cf, incomplete) = CF.getIncompleteChunks chunkField
            nextChunk <- if lastStage
                        then return $ unsafePerformIO $ do
                          rand <- randomRIO (0, Prelude.length incomplete - 1)
                          return $ Just (cf, incomplete !! rand)
                        else return $ CF.getNextChunk chunkField
            case nextChunk of
              Just (chunkField', cix) -> do
                registerCleanup ix cix
                let nextChunkSize = expectedChunkSize totalSize ix (cix+1) pieceLen defaultChunkSize
                    request = Request ix (cix * defaultChunkSize) nextChunkSize
                    modifiedPeer = peerData { requestsLive = requestsLive peerData + 1 }
                runMemory  $ do
                  modifyChunks (Map.insert ix (chunkField', chunkData))
                updatePeerData modifiedPeer
                emit request
                when (requestsLive modifiedPeer < maxRequestsPerPeer) $
                  requestNextPiece
              _ -> processPiece ix >> requestNextPiece
          Nothing -> do
            let chunksCount = chunksInPieces pieceLen defaultChunkSize
                chunkData = B.replicate (fromIntegral pieceLen) 0
                insertion = (CF.newChunkField chunksCount, chunkData)
            runMemory $
              modifyChunks $ Map.insert ix insertion
            requestNextPiece

handlePWP :: PWP -> F PeerMonad ()
handlePWP Unchoke = peerUnchoked >> requestNextPiece
handlePWP (Bitfield field) = handleBitfield field
handlePWP (Piece ix offset d) = receiveChunk ix offset d >> requestNextPiece
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

entryPoint :: F PeerMonad ()
entryPoint = forever $ getPeerEvent >>= handler
  where handler (PWPEvent pwp) = handlePWP pwp
        handler (SharedEvent RequestPiece) = requestNextPiece
        handler (SharedEvent WakeUp) = requestNextPiece
