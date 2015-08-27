{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Network.BitTorrent.Client (
  newClientState
, newPeer
, btListen
, queryTracker
, PeerData(..)
, ClientState(..)
, reachOutToPeer
, mainPeerLoop
, expectedChunkSize
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Crypto.Hash.SHA1
import Data.Binary
import Data.Binary.Get
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Conversion (fromByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.UUID hiding (fromByteString)
import Data.UUID.V4
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Storable.Mutable as VS
-- import Hexdump
import Lens.Family2
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Bencoding.Lenses
import Network.BitTorrent.BitField (BitField)
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.FileWriter as FW
import Network.BitTorrent.MetaInfo as Meta
import qualified Network.BitTorrent.PeerSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Utility
import Network.HTTP.Client
import Network.Socket
import System.FilePath
import System.IO
import System.Random

maxRequestsPerPeer :: Word8
maxRequestsPerPeer = 3

data PeerData = PeerData {
  amChoking :: Bool
, amInterested :: Bool
, peerChoking :: Bool
, peerInterested :: Bool
, address :: SockAddr
, peerId :: ByteString
, peerBitField :: BitField
, chan :: Chan PWP
, requestsLive :: Word8
}

data ClientState = ClientState {
  statePeers :: TVar (Map ByteString PeerData)
, myPeerId :: ByteString
, metaInfo :: MetaInfo
, bitField :: TVar BitField
, pieceChunks :: TVar (Map Word32 (ChunkField, ByteString))
, outputChan :: Chan FW.Operation
, ourPort :: Word16
, availabilityData :: TVar PS.AvailabilityData
}

defaultChunkSize :: Word32
defaultChunkSize = 2 ^ (16 :: Word32)

getPeer :: ByteString -> ClientState -> STM (Maybe PeerData)
getPeer peerId state = Map.lookup peerId <$> readTVar (statePeers state)

setPeer :: ByteString -> PeerData -> ClientState -> STM ()
setPeer peerId peerData state = modifyTVar' (statePeers state) (Map.insert peerId peerData)

newPeer :: Word32 -> SockAddr -> ByteString -> IO PeerData
newPeer pieceCount addr peer = do
  chan <- newChan
  return $ PeerData True False True False addr peer (BF.newBitField pieceCount) chan 0

addToAvailability :: BitField -> ClientState -> STM ()
addToAvailability bf state = do
  avData <- readTVar (availabilityData state)
  let avData' = PS.addToAvailability bf avData
  writeTVar (availabilityData state) avData'

removeFromAvailability :: BitField -> ClientState -> STM ()
removeFromAvailability bf state = do
  avData <- readTVar (availabilityData state)
  let avData' = PS.removeFromAvailability bf avData
  writeTVar (availabilityData state) avData'

newClientState :: FilePath -> MetaInfo -> Word16 -> IO ClientState
newClientState dir meta listenPort = do
  peers <- newTVarIO Map.empty
  chunks <- newTVarIO Map.empty
  uuid <- nextRandom
  let peer = hash $ toASCIIBytes uuid
  let numPieces :: Integral a => a
      numPieces = fromIntegral (B.length $ pieces $ info meta) `quot` 20
  bit_field <- newTVarIO $ BF.newBitField numPieces
  forkIO $ forever $ do
    bf <- atomically $ readTVar bit_field
    print bf
    threadDelay 5000000
  outHandle <- openFile (dir </> BC.unpack (name (info meta))) ReadWriteMode
  outChan <- FW.operate outHandle
  avData <- newTVarIO $ VU.replicate numPieces 0
  return $ ClientState peers peer meta bit_field chunks outChan listenPort avData

btListen :: ClientState -> IO Socket
btListen state = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral $ ourPort state) 0)
  listen sock 10
  forkIO $ forever $ do
    (sock', addr) <- accept sock
    forkIO $ startFromPeerHandshake state sock' addr
  return sock

startFromPeerHandshake :: ClientState -> Socket -> SockAddr -> IO ()
startFromPeerHandshake state sock addr = do
  handle <- socketToHandle sock ReadWriteMode
  (rest, BHandshake hisInfoHash peer) <- readHandshake handle

  ourPeers <- atomically $ readTVar (statePeers state)

  let ourInfoHash = infoHash $ metaInfo state
      cond = hisInfoHash == ourInfoHash && Map.notMember peer ourPeers
      pieceCount = fromIntegral $ (`quot` 20) $ B.length $ pieces $ info $ metaInfo state

  when cond $ do
    writeHandshake handle state
    pData <- newPeer pieceCount addr peer
    atomically $ setPeer peer pData state

    forkIO $ peerEchoer (chan pData) handle

    bf <- atomically $
      readTVar (bitField state)

    writeChan (chan pData) $ Bitfield (BF.raw bf)

    mainPeerLoop state peer handle $ runGetIncremental get `pushChunk` rest

reachOutToPeer :: ClientState -> SockAddr -> IO ()
reachOutToPeer state addr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock addr
  handle <- socketToHandle sock ReadWriteMode

  writeHandshake handle state
  (nextInput, BHandshake _ peer) <- readHandshake handle

  pd <- newPeer (fromIntegral $ (`quot` 20) $ B.length $ pieces $ info $ metaInfo state)
                addr peer
  atomically $
    modifyTVar' (statePeers state) (Map.insert peer pd)
  forkIO $ peerEchoer (chan pd) handle

  mainPeerLoop state peer handle $ runGetIncremental get `pushChunk` nextInput

writeHandshake :: Handle -> ClientState -> IO ()
writeHandshake handle state = BL.hPut handle handshake
  where handshake = encode $ BHandshake (infoHash . metaInfo $ state) (myPeerId state)

readHandshake :: Handle -> IO (ByteString, BHandshake)
readHandshake handle = evaluator (runGetIncremental get)
  where evaluator Fail{} = do
          putStrLn "getHandshake: Their handshake is a phail"
          error "getHandshake: Their handshake is a phail"
        evaluator parser@(Partial _) = do
          hWaitForInput handle (-1)
          input <- B.hGetSome handle 1024
          evaluator $ parser `pushChunk` input
        evaluator (Done unused _ handshake) =
          return (unused `seq` handshake `seq` (unused, handshake))

peerEchoer :: Chan PWP -> Handle -> IO ()
peerEchoer chan handle = forever $ do
  msg <- readChan chan
  BL.hPut handle $ encode msg

mainPeerLoop :: ClientState -> ByteString -> Handle -> Decoder PWP -> IO ()
mainPeerLoop state peer handle parser =
  case parser of
    Fail _ _ err -> do
      putStrLn "mainPeerLoop fail parse"
      putStrLn err
    Partial _ -> do
      -- this improves test performance *greatly*
      -- for some reason we can read absolutely nothing
      -- many times in a row from local
      -- sockets with hGetSome
      {-# SCC "waiting-on-input" #-} hWaitForInput handle (-1)
      input <- B.hGetSome handle 1024
      mainPeerLoop state peer handle (parser `pushChunk` input)
    Done unused _ msg -> do
      handleMessage state peer msg
      let replacementParser = runGetIncremental get `pushChunk` unused
      mainPeerLoop state peer handle replacementParser

chunksInPieces :: Word32 -> Word32 -> Word32
chunksInPieces = divideSize
{-# INLINABLE chunksInPieces #-}

handleMessage :: ClientState -> ByteString -> PWP -> IO ()
handleMessage state peer msg = do
  peers <- atomically $ readTVar $ statePeers state
  case Map.lookup peer peers of
    Just peerData -> act peerData msg
    Nothing -> return ()
  where
    -- totalSize = Meta.length $ info $ metaInfo state
    defaultPieceLen = pieceLength $ info $ metaInfo state
    emit peerData = writeChan (chan peerData)
    act _ Unchoke = requestNextPiece state peer
    act peerData (Bitfield field) = do
      -- take our bitfield length
      bf <- atomically $ readTVar $ bitField state
      let bitField = BF.BitField field (BF.length bf)
      let peerData' = peerData { peerBitField = bitField }
      atomically $ do
        setPeer peer peerData' state
        addToAvailability bitField state
      emit peerData Interested
    act _ (Piece ix offset d) = do
      -- let pieceLen = expectedPieceSize totalSize ix defaultPieceLen
          -- chunkIndex = divideSize offset defaultChunkSize
          -- chunkSize = expectedChunkSize totalSize ix chunkIndex pieceLen defaultChunkSize

      -- print $ "got piece " <> show ix <> " at " <> show offset
      -- print "mam piece"
      -- print $ "pieceLen " <> show pieceLen
      -- print chunkIndex
      -- print chunkSize
      -- print $ B.length d
      {-if (fromIntegral $ B.length d) == chunkSize
        then print "i pasuje mi"
        else print "cos nie tak"-}
      -- chunks <- atomically $
        -- readTVar (pieceChunks state)
      -- print $ B.length <$> Map.lookup ix chunks
      -- print offset

      chunkField <- atomically $ do
        chunks <- readTVar (pieceChunks state)
        return $ Map.lookup ix chunks

      case chunkField of
        Just (_, chunkData) -> do
          let (ptr, o, len) = BI.toForeignPtr chunkData
              chunkVector = VS.unsafeFromForeignPtr ptr o len
              (ptr', o', len') = BI.toForeignPtr d
              dataVector = VS.unsafeFromForeignPtr ptr' o' len'
              dest = VS.take (B.length d) $ VS.drop (fromIntegral offset) chunkVector
              src = dataVector
          VS.copy dest src
        Nothing -> return ()

      chunkField <- atomically $ do
        chunks <- readTVar (pieceChunks state)
        case Map.lookup ix chunks of
          Just (chunkField, chunkData) -> do
            let chunkIndex = divideSize offset defaultChunkSize
                chunkField' = CF.markCompleted chunkField chunkIndex

            modifyTVar' (pieceChunks state) $ Map.insert ix (chunkField', chunkData)
            return $ Just chunkField'
          {- ignore when someone already DL'd this piece. WASTE
            Just chunk -> return ()
            -}
          _ -> return Nothing -- someone already filled this

      case chunkField of
        Just cf -> processPiece state ix cf
        Nothing -> return ()

      requestNextPiece state peer
    act peerData msg@(Have ix) = do
      let peerData' = peerData { peerBitField = BF.set (peerBitField peerData) ix True }
      atomically $ do
        setPeer peer peerData' state
        removeFromAvailability (peerBitField peerData) state
        addToAvailability (peerBitField peerData') state
    act peerData Interested | amChoking peerData = do
      emit peerData Unchoke
      let peerData' = peerData { amChoking = False }
      atomically $ setPeer peer peerData' state
    act peerData (Request pid offset len) | not (amChoking peerData) = do
      let action d = emit peerData $ Piece pid offset d
      -- print $ "SENDING " <> show len <> " BYTES"
      writeChan (outputChan state) $
        FW.ReadBlock (pid * defaultPieceLen + offset) len action
    act _ m = do
      putStrLn "unhandled Message"
      print m

expectedPieceSize :: Word32 -> Word32 -> Word32 -> Word32
expectedPieceSize totalSize pix pSize =
  if pix >= pCount
    then if totalSize `rem` pSize == 0
         then pSize
         else totalSize `rem` pSize
    else pSize
  where pCount = divideSize totalSize pSize - 1

expectedChunkSize :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
expectedChunkSize totalSize pix cix pSize cSize =
  if cix >= chunksInPiece
    then if expectedPSize `rem` cSize == 0
         then cSize
         else expectedPSize `rem` cSize
    else cSize
  where expectedPSize = expectedPieceSize totalSize pix pSize
        chunksInPiece = chunksInPieces expectedPSize cSize

requestNextPiece :: ClientState -> ByteString -> IO ()
requestNextPiece state peer = do
  (chunks, Just peerData, bf, avData) <- atomically $ do
    chunks <- readTVar (pieceChunks state)
    peerData <- getPeer peer state
    avData <- readTVar (availabilityData state)
    bf <- readTVar (bitField state)
    return (chunks, peerData, bf, avData)
  let c = chan peerData
  let pbf = peerBitField peerData
  -- let startedPieces = Map.keys chunks
  -- let weights = filter (not . BF.get bf . snd) $
  --              filter (BF.get pbf . snd) $
  --              (PS.buildWeights avData 250 startedPieces)
  {-
  putStrLn $ "maximum weight " <> show (Prelude.maximum (snd <$> weights)) <>
            " length of  avData " <> show (VU.length avData)
  putStrLn $ prettyHex (BF.raw pbf)
  print pbf
  print $ BF.get pbf 1285
  print $ BF.get pbf 1286
  print $ BF.get pbf 1287
  print $ BF.get pbf 1288
  print $ BF.get pbf 1289
  print $ BF.get pbf 1290
  print $ BF.get pbf 1291
  print $ BF.get pbf 1292
  print $ BF.get pbf 1293
  print $ BF.get pbf 1294
  print $ BF.get pbf 1295
  print avData
  print startedPieces
  print (snd <$> PS.buildWeights avData 250 startedPieces)
  print $ snd <$> (filter (BF.get pbf . snd) $
          (PS.buildWeights avData 250 startedPieces))
  print $ snd <$> weights
  exitWith (ExitFailure 1)
  -}
  -- nextPiece <- runRVar (weightedSample 1 weights) StdRandom

  let defaultPieceLen :: Word32
      defaultPieceLen = pieceLength $ info $ metaInfo state
      infoDict = info $ metaInfo state
      pieces' = pieces infoDict
      -- pieceCount :: Integral a => a
      -- pieceCount = fromIntegral $ (`quot`20) $ B.length pieces'
      totalSize = Meta.length infoDict
      getPieceHash ix = B.take 20 $ B.drop (fromIntegral ix * 20) pieces'

  let lastStage = BF.completed bf > 0.9
  -- putStrLn $ "lastStage= " <> show lastStage
  -- putStrLn $ "completed= " <> show (BF.completed bf)
  let bfrequestable = if lastStage
                        then bf
                        else Map.foldlWithKey' (\bit ix (cf, _) ->
                               if not (BF.get pbf ix) || CF.isRequested cf
                                 then BF.set bit ix True
                                 else bit) bf chunks
  {-
   - putStrLn "BF vs BF Requestable"
   - print (fmap fst <$> Map.elems chunks)
   - print bf
   - print bfrequestable
   -}

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
          -- let cix = divideSize (fromIntegral $ B.length d) defaultChunkSize
          -- print $ "data for piece " <> show ix
          -- print $ "cix=" <> show cix
          -- print $ "we have already " <> show (B.length d)
          -- print $ chunksInPieces pieceLen defaultChunkSize
          -- print $ "nextChunkSize=" <> show nextChunkSize
          -- print $ "totalSize=" <> show totalSize
          -- print $ "pix=" <> show ix
          -- print $ "cix=" <> show cix
          -- print $ "pieceLen=" <> show pieceLen
          -- print $ "defaultChunkSize=" <> show defaultChunkSize
          -- TUTEJ
          -- cix < (chunksInPieces pieceLen defaultChunkSize)
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
              putStrLn $ "requesting " <> show request
              atomically $ do
                setPeer peer modifiedPeer state
                modifyTVar' (pieceChunks state) $ Map.insert ix (chunkField', chunkData)
              writeChan c request
              when (requestsLive modifiedPeer < maxRequestsPerPeer) $
                requestNextPiece state peer
            _ -> processPiece state ix chunkField >> requestNextPiece state peer
        Nothing -> do
          let chunksCount = chunksInPieces pieceLen defaultChunkSize
              chunkData = B.replicate (fromIntegral pieceLen) 0
              insertion = (CF.newChunkField chunksCount, chunkData)
          atomically $
            modifyTVar' (pieceChunks state) $ Map.insert ix insertion
          requestNextPiece state peer


processPiece :: ClientState -> Word32 -> CF.ChunkField -> IO ()
processPiece _ _ chunkField | not (CF.isCompleted chunkField) = return ()
processPiece state ix chunkField =  do
  Just d <- atomically $ do
    map <- readTVar (pieceChunks state)
    return (snd <$> Map.lookup ix map)
  let infoDict = info $ metaInfo state
      pieces' = pieces infoDict
      defaultPieceLen = pieceLength $ info $ metaInfo state
      getPieceHash ix = B.take 20 $ B.drop (fromIntegral ix * 20) pieces'
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
    print $ "writing " <> show ix
    writeChan outChan $ FW.WriteBlock (defaultPieceLen * ix) d
    return ()

queryTracker :: ClientState -> IO ()
queryTracker state = do
  let meta = metaInfo state
  let url = fromByteString (announce meta) >>= parseUrl
  let req = setQueryString [ ("peer_id", Just (myPeerId state))
                           , ("info_hash", Just . infoHash $ meta)
                           , ("compact", Just "1")
                           , ("port", Just "8035")
                           ] (fromJust url)

  putStrLn "pieceCount"
  print $ (`quot`20) $ B.length $ pieces $ info meta
  avData <- atomically $ readTVar $ availabilityData state
  print $ VU.length avData
  putStrLn "pieceLen"
  print $ pieceLength $ info meta

  withManager defaultManagerSettings $ \manager -> do
    response <- httpLbs req manager
    case AC.maybeResult (AC.parse value (BL.toStrict $ responseBody response)) of
      Just v -> do
        let peers = getPeers $ BL.fromStrict $ v ^. (bkey "peers" . bstring)
        _ <- traverse (forkIO . reachOutToPeer state) peers
        threadDelay 100000000
      _ -> Prelude.putStrLn "can't parse response"

getPeers :: BL.ByteString -> [SockAddr]
getPeers src | BL.null src = []
getPeers src = SockAddrInet port' ip : getPeers (BL.drop 6 src)
               where slice = BL.take 6 src
                     ipRaw = BL.take 4 slice
                     ip = runGet getWord32le ipRaw -- source is actually network order,
                                                   -- but HostAddress is too and Data.Binary
                                                   -- converts in `runGet`
                                                   -- we're avoiding this conversion
                     portSlice = BL.drop 4 slice
                     port' = fromIntegral (decode portSlice :: Word16)

