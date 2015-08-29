{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Network.BitTorrent.PeerMonad (
  runTorrent
, serveChunk
, receiveChunk
, processPiece
, requestNextPiece
, handleUnchoke
, handleBitfield
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Free
import Control.Monad.STM
import Crypto.Hash.SHA1
import Data.Binary
import qualified Data.ByteString as B
import Data.ByteString.Internal as BI
import Data.Map.Strict as Map
import Data.Monoid
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

type Chunks = Map Word32 (ChunkField, ByteString)

data TorrentSTM a = GetChunks (Chunks -> a)
                  | ModifyChunks (Chunks -> Chunks) a
                  | SetPeer ByteString PeerData a
                  | ReadBitfield (BF.BitField -> a)
                  | ReadAvailability (AvailabilityData -> a)
                  | ModifyAvailability (AvailabilityData -> AvailabilityData) a
                  deriving(Functor)

getChunks :: Free TorrentSTM Chunks
getChunks = liftF $ GetChunks id

modifyChunks :: (Chunks -> Chunks) -> Free TorrentSTM ()
modifyChunks mut = liftF $ ModifyChunks mut ()

setPeer :: ByteString -> PeerData -> Free TorrentSTM ()
setPeer peer peerData = liftF $ SetPeer peer peerData ()

getBitfield :: Free TorrentSTM BF.BitField
getBitfield = liftF $ ReadBitfield id

readAvailability :: Free TorrentSTM AvailabilityData
readAvailability = liftF $ ReadAvailability id

modifyAvailability :: (AvailabilityData -> AvailabilityData) -> Free TorrentSTM ()
modifyAvailability mut = liftF $ ModifyAvailability mut ()

runTorrentSTM :: ClientState -> Free TorrentSTM a -> STM a
runTorrentSTM _ (Pure a) = return a
runTorrentSTM state (Free (GetChunks f)) = do
  chunks <- readTVar (pieceChunks state)
  runTorrentSTM state $ f chunks
runTorrentSTM state (Free (ModifyChunks f t)) = do
  modifyTVar' (pieceChunks state) f
  runTorrentSTM state t
runTorrentSTM state (Free (SetPeer peer peerData t)) = do
  modifyTVar' (statePeers state) (Map.insert peer peerData)
  runTorrentSTM state t
runTorrentSTM state (Free (ReadBitfield next)) = do
  res <- readTVar (bitField state)
  runTorrentSTM state (next res)
runTorrentSTM state (Free (ReadAvailability next)) = do
  res <- readTVar (availabilityData state)
  runTorrentSTM state (next res)
runTorrentSTM state (Free (ModifyAvailability mut next)) = do
  modifyTVar' (availabilityData state) mut
  runTorrentSTM state next

data TorrentM a = ServeChunk Word32 Word32 Word32 a
                | ReceiveChunk Word32 Word32 ByteString a
                | ProcessPiece Word32 a
                | RequestNextPiece a
                | PeerUnchoked a
                | forall b. RunSTM (Free TorrentSTM b) (b -> a)
                | GetPeerData (PeerData -> a)
                | Emit PWP a

instance Functor TorrentM where
  fmap f (ServeChunk a b c next) = ServeChunk a b c (f next)
  fmap f (ReceiveChunk a b d next) = ReceiveChunk a b d (f next)
  fmap f (ProcessPiece a next) = ProcessPiece a (f next)
  fmap f (RequestNextPiece next) = RequestNextPiece (f next)
  fmap f (PeerUnchoked next) = PeerUnchoked (f next)
  fmap f (RunSTM action next) = RunSTM action (fmap f next)
  fmap f (GetPeerData next) = GetPeerData (fmap f next)
  fmap f (Emit pwp next) = Emit pwp (f next)

serveChunk :: Word32 -> Word32 -> Word32 -> Free TorrentM ()
serveChunk a b c = liftF $ ServeChunk a b c ()
{-# INLINABLE serveChunk #-}

receiveChunk :: Word32 -> Word32 -> ByteString -> Free TorrentM ()
receiveChunk a b d = liftF $ ReceiveChunk a b d ()
{-# INLINABLE receiveChunk #-}

processPiece :: Word32 -> Free TorrentM ()
processPiece a = liftF $ ProcessPiece a ()
{-# INLINABLE processPiece #-}

requestNextPiece :: Free TorrentM ()
requestNextPiece = liftF $ RequestNextPiece ()
{-# INLINABLE requestNextPiece #-}

peerUnchoked :: Free TorrentM ()
peerUnchoked = liftF $ PeerUnchoked ()
{-# INLINABLE peerUnchoked #-}

runSTM :: Free TorrentSTM a -> Free TorrentM a
runSTM stm = liftF $ RunSTM stm id
{-# INLINABLE runSTM #-}

getPeerData :: Free TorrentM PeerData
getPeerData = liftF $ GetPeerData id
{-# INLINABLE getPeerData #-}

getPeerHash :: Free TorrentM ByteString
getPeerHash = peerId <$> getPeerData
{-# INLINABLE getPeerHash #-}

emit :: PWP -> Free TorrentM ()
emit pwp = liftF $ Emit pwp ()
{-# INLINABLE emit #-}

handleBitfield :: ByteString -> Free TorrentM ()
handleBitfield field = do
  peerData <- getPeerData
  runSTM $ do
    len <- BF.length <$> getBitfield
    let newBitField = BF.BitField field len
        peer = peerId peerData
        peerData' = peerData { peerBitField = newBitField }
    setPeer peer peerData'
    modifyAvailability $ PS.addToAvailability newBitField
  emit Interested

runTorrent :: ClientState -> ByteString -> Free TorrentM a -> IO a
runTorrent state peerHash t = do
  Just peerData <- atomically $
    Map.lookup peerHash <$> readTVar (statePeers state)
  evalTorrent state peerData t
{-# INLINABLE runTorrent #-}

handleUnchoke :: Free TorrentM ()
handleUnchoke = peerUnchoked >> requestNextPiece

evalTorrent :: ClientState -> PeerData -> Free TorrentM a -> IO a
evalTorrent _ _ (Pure a) = return a
evalTorrent state peerData (Free (ServeChunk ix offset len t)) = do
  unless (amChoking peerData) $ do
    writeChan (outputChan state) $
      FW.ReadBlock (ix * defaultPieceLen + offset) len action
  evalTorrent state peerData t
  where defaultPieceLen = pieceLength $ info $ metaInfo state
        action d = writeChan (chan peerData) (Piece ix offset d)
evalTorrent state peerData (Free (ReceiveChunk ix offset d t)) = do
  chunkField <- atomically $ runTorrentSTM state $ do
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

  if chunkField
    then evalTorrent state peerData (processPiece ix)
    else return ()

  evalTorrent state peerData t
evalTorrent state peerData (Free (ProcessPiece ix t)) = do
  Just (chunkField, d) <- evalTorrent state peerData (runSTM (Map.lookup ix <$> getChunks))
  when (CF.isCompleted chunkField) $ do
    let infoDict = info $ metaInfo state
        pieces' = pieces infoDict
        defaultPieceLen = pieceLength $ info $ metaInfo state
        getPieceHash n = B.take 20 $ B.drop (fromIntegral n * 20) pieces'
        hashCheck = hash d == getPieceHash ix

    unless hashCheck $ do
      print $ "Validating hashes " <> show hashCheck
      print ix

    wasSetAlready <- atomically $ do
      modifyTVar' (pieceChunks state) (Map.delete ix)
      if hashCheck
        then do
          bf <- readTVar (bitField state)
          let wasSetAlready = BF.get bf ix
          unless wasSetAlready $
            modifyTVar' (bitField state) (\bf' -> BF.set bf' ix True)
          return wasSetAlready
        else return False
      -- because we remove the entry from (pieceChunks state),
      -- but not indicate it as downloaded in the bitField,
      -- it will be reacquired again

    when (hashCheck && not wasSetAlready) $ do
      let outChan = outputChan state
      -- print $ "writing " <> show ix
      writeChan outChan $ FW.WriteBlock (defaultPieceLen * ix) d
      return ()

  evalTorrent state peerData t
evalTorrent state peerData (Free (RequestNextPiece t)) = do
  unless (peerChoking peerData) $ do
    (chunks, bf, avData) <- atomically $ do
      chunks <- readTVar (pieceChunks state)
      avData <- readTVar (availabilityData state)
      bf <- readTVar (bitField state)
      return (chunks, bf, avData)
    let peer = peerId peerData
        c = chan peerData
        pbf = peerBitField peerData
    let defaultPieceLen :: Word32
        defaultPieceLen = pieceLength $ info $ metaInfo state
        infoDict = info $ metaInfo state
        totalSize = Meta.length infoDict

    let lastStage = BF.completed bf > 0.9
    let bfrequestable = if lastStage
                          then bf
                          else Map.foldlWithKey' (\bit ix (cf, _) ->
                                 if not (BF.get pbf ix) || CF.isRequested cf
                                   then BF.set bit ix True
                                   else bit) bf chunks

    let incompletePieces = PS.getIncompletePieces bfrequestable

    nextPiece <- if lastStage
                   then do
                     if Prelude.length incompletePieces - 1 > 0
                       then do
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
                        then do
                          rand <- randomRIO (0, Prelude.length incomplete - 1)
                          return $ Just (cf, incomplete !! rand)
                        else return $ CF.getNextChunk chunkField
            case nextChunk of
              Just (chunkField', cix) -> do
                let nextChunkSize = expectedChunkSize totalSize ix (cix+1) pieceLen defaultChunkSize
                    request = Request ix (cix * defaultChunkSize) nextChunkSize
                    modifiedPeer = peerData { requestsLive = requestsLive peerData + 1 }
                -- putStrLn $ "requesting " <> show request
                atomically $ runTorrentSTM state $ do
                  setPeer peer modifiedPeer
                  modifyChunks (Map.insert ix (chunkField', chunkData))
                writeChan c request
                when (requestsLive modifiedPeer < maxRequestsPerPeer) $
                  runTorrent state peer requestNextPiece
              _ -> runTorrent state peer (processPiece ix >> requestNextPiece)
          Nothing -> do
            let chunksCount = chunksInPieces pieceLen defaultChunkSize
                chunkData = B.replicate (fromIntegral pieceLen) 0
                insertion = (CF.newChunkField chunksCount, chunkData)
            atomically $ runTorrentSTM state $
              modifyChunks $ Map.insert ix insertion
            runTorrent state peer requestNextPiece

  evalTorrent state peerData t
evalTorrent state peerData (Free (PeerUnchoked t)) = do
  let peer = peerId peerData
      peerData' = peerData { peerChoking = False }
  atomically $ runTorrentSTM state $
    setPeer peer peerData'
  runTorrent state (peerId peerData) t
evalTorrent state peerData (Free (RunSTM a next)) = do
  res <- atomically $ runTorrentSTM state a
  evalTorrent state peerData (next res)
evalTorrent state peerData (Free (GetPeerData next)) = do
  evalTorrent state peerData (next peerData)
evalTorrent state peerData (Free (Emit pwp next)) = do
  writeChan (chan peerData) pwp
  evalTorrent state peerData next
