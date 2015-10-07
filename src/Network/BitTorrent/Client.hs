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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal as BI
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Conversion (fromByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.UUID hiding (fromByteString)
import Data.UUID.V4
import qualified Data.Vector.Storable as VS
-- import Hexdump
import Lens.Family2
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Bencoding.Lenses
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.PeerMonad
import Network.BitTorrent.PWP
import Network.BitTorrent.Types
import Network.HTTP.Client
import Network.Socket
import System.FilePath
import System.IO

newPeer :: Word32 -> SockAddr -> ByteString -> Handle -> IO PeerData
newPeer pieceCount addr peer handle =
  return $ PeerData True False True False addr peer (BF.newBitField pieceCount) 0 handle
{-# INLINABLE newPeer #-}

newClientState :: FilePath -> MetaInfo -> Word16 -> IO ClientState
newClientState dir meta listenPort = do
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
  avData <- newTVarIO $ VS.replicate numPieces 0
  mvar <- newMVar ()
  return $ ClientState peer meta bit_field chunks outHandle mvar listenPort avData

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
  (nextInput, BHandshake hisInfoHash peer) <- readHandshake handle


  let ourInfoHash = infoHash $ metaInfo state
      cond = hisInfoHash == ourInfoHash
      -- && Map.notMember peer ourPeers -- for later
      pieceCount = fromIntegral $ (`quot` 20) $ B.length $ pieces $ info $ metaInfo state

  when cond $ do
    writeHandshake handle state
    pData <- newPeer pieceCount addr peer handle

    bf <- atomically $
      readTVar (bitField state)

    BL.hPut handle (encode $ Bitfield $ BF.raw bf)

    mainPeerLoop state pData nextInput

reachOutToPeer :: ClientState -> SockAddr -> IO ()
reachOutToPeer state addr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock addr
  handle <- socketToHandle sock ReadWriteMode

  writeHandshake handle state
  (nextInput, BHandshake _ peer) <- readHandshake handle

  let pieceCount = fromIntegral $ (`quot` 20) $ B.length $ pieces $ info $ metaInfo state
  pd <- newPeer pieceCount addr peer handle

  mainPeerLoop state pd nextInput

writeHandshake :: Handle -> ClientState -> IO ()
writeHandshake handle state = BL.hPut handle handshake
  where handshake = encode $ BHandshake (infoHash . metaInfo $ state) (myPeerId state)
{-# INLINABLE writeHandshake #-}

readHandshake :: Handle -> IO (BL.ByteString, BHandshake)
readHandshake handle = do
  input <- BL.hGetContents handle
  case runGetOrFail get input of
    Left _ ->
      error "getHandshake: Their handshake is a phail"
    Right (unused, _, handshake) ->
      return (unused, handshake)
{-# INLINABLE readHandshake #-}

peerEchoer :: Chan PWP -> Handle -> IO ()
peerEchoer c h = forever $ do
  msg <- readChan c
  BL.hPut h $ encode msg

mainPeerLoop :: ClientState -> PeerData -> BL.ByteString -> IO ()
mainPeerLoop state pData input =
  runTorrent state pData (entryPoint $ messageStream input)

messageStream :: BL.ByteString -> [PWP]
messageStream input =
  case runGetOrFail get input of
    Left _ -> []
    Right (rest, _, msg) -> msg : messageStream rest

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
  print $ VS.length avData
  putStrLn "pieceLen"
  print $ pieceLength $ info meta

  manager <- newManager defaultManagerSettings
  response <- httpLbs req manager
  case AC.maybeResult (AC.parse value (BL.toStrict $ responseBody response)) of
    Just v -> do
      let peers = getPeers $ BL.fromStrict $ v ^. (bkey "peers" . bstring)
      _ <- traverse (forkIO . reachOutToPeer state) peers
      threadDelay 100000000
    _ -> Prelude.putStrLn "can't parse response"
{-# INLINABLE queryTracker #-}

getPeers :: BL.ByteString -> [SockAddr]
getPeers src | BL.null src = []
getPeers src = SockAddrInet port' ip : getPeers (BL.drop 6 src)
               where chunk = BL.take 6 src
                     ipRaw = BL.take 4 chunk
                     ip = runGet getWord32le ipRaw -- source is actually network order,
                                                   -- but HostAddress is too and Data.Binary
                                                   -- converts in `runGet`
                                                   -- we're avoiding this conversion
                     portSlice = BL.drop 4 chunk
                     port' = fromIntegral (decode portSlice :: Word16)
{-# INLINABLE getPeers #-}
