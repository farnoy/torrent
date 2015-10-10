{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Network.BitTorrent.PeerMonad (
  runTorrent
, entryPoint
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Strict
import Crypto.Hash.SHA1
import qualified Data.Binary as Binary
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
import System.IO.Unsafe
import System.Random hiding(next)

data PeerState = PeerState { peerStateData :: PeerData
                           , peerStateChan :: Chan PeerEvent
                           }

-- TODO Logger
type PeerMonad = ReaderT ClientState (StateT PeerState IO)

type Chunks = Map Word32 (ChunkField, ByteString)

data TorrentSTM a = GetChunks (Chunks -> a)
                  | ModifyChunks (Chunks -> Chunks) a
                  | ReadBitfield (BF.BitField -> a)
                  | ModifyBitfield (BF.BitField -> BF.BitField) a
                  | ReadAvailability (AvailabilityData -> a)
                  | ModifyAvailability (AvailabilityData -> AvailabilityData) a
                  deriving(Functor)

getChunks :: F TorrentSTM Chunks
getChunks = liftF $ GetChunks id
{-# INLINABLE getChunks #-}

modifyChunks :: (Chunks -> Chunks) -> F TorrentSTM ()
modifyChunks mut = liftF $ ModifyChunks mut ()
{-# INLINABLE modifyChunks #-}

getBitfield :: F TorrentSTM BF.BitField
getBitfield = liftF $ ReadBitfield id
{-# INLINABLE getBitfield #-}

modifyBitfield :: (BF.BitField -> BF.BitField) -> F TorrentSTM ()
modifyBitfield mut = liftF $ ModifyBitfield mut ()
{-# INLINABLE modifyBitfield #-}

getAvailability :: F TorrentSTM AvailabilityData
getAvailability = liftF $ ReadAvailability id
{-# INLINABLE getAvailability #-}

modifyAvailability :: (AvailabilityData -> AvailabilityData) -> F TorrentSTM ()
modifyAvailability mut = liftF $ ModifyAvailability mut ()
{-# INLINABLE modifyAvailability #-}

evalTorrentSTM :: ClientState -> TorrentSTM (STM a) -> STM a
evalTorrentSTM state (GetChunks next) = do
  chunks <- readTVar (pieceChunks state)
  next chunks
evalTorrentSTM state (ModifyChunks f next) = do
  modifyTVar' (pieceChunks state) f
  next
evalTorrentSTM state (ReadBitfield next) = do
  res <- readTVar (bitField state)
  next res
evalTorrentSTM state (ModifyBitfield mut next) = do
  modifyTVar' (bitField state) mut
  next
evalTorrentSTM state (ReadAvailability next) = do
  res <- readTVar (availabilityData state)
  next res
evalTorrentSTM state (ModifyAvailability mut next) = do
  modifyTVar' (availabilityData state) mut
  next

runTorrentSTM :: ClientState -> F TorrentSTM a -> STM a
runTorrentSTM state = iterM (evalTorrentSTM state)

data PeerEvent = PWPEvent PWP | SharedEvent SharedMessage

data TorrentM a = forall b. RunSTM (F TorrentSTM b) (b -> a)
                | GetPeerData (PeerData -> a)
                | Emit PWP a
                | GetMeta (MetaInfo -> a)
                | ReadData Word32 Word32 (ByteString -> a)
                | WriteData Word32 ByteString a
                | UpdateState PeerData a
                | GetPeerEvent (PeerEvent -> a)

instance Functor TorrentM where
  fmap f (RunSTM action next) = RunSTM action (fmap f next)
  fmap f (GetPeerData next) = GetPeerData (fmap f next)
  fmap f (Emit pwp next) = Emit pwp (f next)
  fmap f (GetMeta next) = GetMeta (fmap f next)
  fmap f (ReadData o l next) = ReadData o l (fmap f next)
  fmap f (WriteData o b next) = WriteData o b (f next)
  fmap f (UpdateState pData next) = UpdateState pData (f next)
  fmap f (GetPeerEvent next) = GetPeerEvent (fmap f next)

peerUnchoked :: F TorrentM ()
peerUnchoked = do
  peerData <- getPeerData
  updateState $ peerData { peerChoking = False }
{-# INLINABLE peerUnchoked #-}

runSTM :: F TorrentSTM a -> F TorrentM a
runSTM stm = liftF $ RunSTM stm id
{-# INLINABLE runSTM #-}

getPeerData :: F TorrentM PeerData
getPeerData = liftF $ GetPeerData id
{-# INLINABLE getPeerData #-}

emit :: PWP -> F TorrentM ()
emit pwp = liftF $ Emit pwp ()
{-# INLINABLE emit #-}

getMeta :: F TorrentM MetaInfo
getMeta = liftF $ GetMeta id
{-# INLINABLE getMeta #-}

readData :: Word32 -> Word32 -> F TorrentM ByteString
readData o l = liftF $ ReadData o l id
{-# INLINABLE readData #-}

writeData :: Word32 -> ByteString -> F TorrentM ()
writeData o b = liftF $ WriteData o b ()
{-# INLINABLE writeData #-}

updateState :: PeerData -> F TorrentM ()
updateState pData = liftF $ UpdateState pData ()

getPeerEvent :: F TorrentM PeerEvent
getPeerEvent = liftF $ GetPeerEvent id

runTorrent :: ClientState -> PeerData -> [PWP] -> F TorrentM a -> IO a
runTorrent state pData messages t = do
  chan <- newChan
  let peerState = PeerState pData chan
  void $ forkIO $ traverse_ (writeChan chan . PWPEvent) messages
  sharedChan <- dupChan (sharedMessages state)
  void $ forkIO $ forever $ readChan sharedChan >>= writeChan chan . SharedEvent
  -- TODO: store this threadId for killing later
  evalStateT (runReaderT (inside t) state) peerState
  where inside = iterM evalTorrent
{-# INLINABLE runTorrent #-}

evalTorrent :: TorrentM (PeerMonad a) -> PeerMonad a
evalTorrent (RunSTM a next) = do
  state <- ask
  res <- liftIO $ atomically $ runTorrentSTM state a
  next res
evalTorrent (GetPeerData next) = do
  PeerState pData _ <- get
  next pData
evalTorrent (Emit pwp next) = do
  PeerState pData _ <- get
  liftIO $ BL.hPut (handle pData) (Binary.encode pwp)
  next
evalTorrent (GetMeta next) = do
  meta <- metaInfo <$> ask
  next meta
evalTorrent (ReadData o l next) = do
  state <- ask
  let hdl = outputHandle state
      lock = outputLock state
  a <- liftIO $ FW.read hdl lock o l
  next a
evalTorrent (WriteData o d next) = do
  state <- ask
  let hdl = outputHandle state
      lock = outputLock state
  liftIO $ FW.write hdl lock o d
  next
evalTorrent (UpdateState pData next) = do
  pState <- get
  put $ pState { peerStateData = pData }
  next
evalTorrent (GetPeerEvent next) = do
  pState <- get
  msg <- liftIO $ readChan $ peerStateChan pState
  next msg

receiveChunk :: Word32 -> Word32 -> ByteString -> F TorrentM ()
receiveChunk ix offset d = do
  chunkField <- runSTM $ do
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
        let chunkIndex = divideSize offset defaultChunkSize
            chunkField' = CF.markCompleted chunkField chunkIndex

        modifyChunks $ Map.insert ix (chunkField', chunkData)
        return True
      _ -> return False -- someone already filled this

  pData <- getPeerData
  updateState (pData { requestsLive = requestsLive pData - 1 })
  when chunkField $ processPiece ix

processPiece :: Word32 -> F TorrentM ()
processPiece ix = do
  Just (chunkField, d) <- runSTM (Map.lookup ix <$> getChunks)
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

    wasSetAlready <- runSTM $ do
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

handleBitfield :: ByteString -> F TorrentM ()
handleBitfield field = do
  peerData <- getPeerData
  newBitField <- runSTM $ do
    len <- BF.length <$> getBitfield
    let newBitField = BF.BitField field len
    modifyAvailability $ PS.addToAvailability newBitField
    return newBitField
  updateState $ peerData { peerBitField = newBitField }
  emit Interested

handleHave :: Word32 -> F TorrentM ()
handleHave ix = do
  peerData <- getPeerData
  let peerData' = peerData { peerBitField = BF.set (peerBitField peerData) ix True }
  runSTM $ do
    let oldBf = peerBitField peerData
        newBf = peerBitField peerData'
    modifyAvailability $ PS.addToAvailability newBf . PS.removeFromAvailability oldBf
  updateState peerData'

handleInterested :: F TorrentM ()
handleInterested = do
  peerData <- getPeerData
  when (amChoking peerData) $ do
    emit Unchoke
    let peerData' = peerData { amChoking = False }
    updateState peerData'

requestNextPiece :: F TorrentM ()
requestNextPiece = do
  peerData <- getPeerData
  unless (peerChoking peerData) $ do
    (chunks, bf, avData) <- runSTM $ do
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
                let nextChunkSize = expectedChunkSize totalSize ix (cix+1) pieceLen defaultChunkSize
                    request = Request ix (cix * defaultChunkSize) nextChunkSize
                    modifiedPeer = peerData { requestsLive = requestsLive peerData + 1 }
                -- putStrLn $ "requesting " <> show request
                runSTM  $ do
                  modifyChunks (Map.insert ix (chunkField', chunkData))
                updateState modifiedPeer
                emit request
                when (requestsLive modifiedPeer < maxRequestsPerPeer) $
                  requestNextPiece
              _ -> processPiece ix >> requestNextPiece
          Nothing -> do
            let chunksCount = chunksInPieces pieceLen defaultChunkSize
                chunkData = B.replicate (fromIntegral pieceLen) 0
                insertion = (CF.newChunkField chunksCount, chunkData)
            runSTM $
              modifyChunks $ Map.insert ix insertion
            requestNextPiece

handlePWP :: PWP -> F TorrentM ()
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

entryPoint :: F TorrentM ()
entryPoint = forever $ getPeerEvent >>= handler
  where handler (PWPEvent pwp) = handlePWP pwp
        handler (SharedEvent RequestPiece) = requestNextPiece
