{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Implements most of the connection handling
-- and initiates peer loops.
module Network.BitTorrent.Client (
  newGlobalState
, newTorrentState
, addActiveTorrent
, newPeer
, btListen
, queryTracker
, reachOutToPeer
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Crypto.Hash.SHA1
import Data.Binary
import Data.Binary.Get
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Conversion (fromByteString)
import Data.Foldable (find)
import qualified Data.IntSet as IntSet
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.UUID (toASCIIBytes)
import Data.UUID.V4
import Lens.Family2
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Bencoding.Lenses
import qualified Network.BitTorrent.BitField as BF
import qualified Network.BitTorrent.DownloadProgress as DP
import Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.PeerMonad
import Network.BitTorrent.PWP
import Network.BitTorrent.Types
import Network.BitTorrent.Utility
import Network.HTTP.Client
import Network.Socket
import System.FilePath
import System.IO

newGlobalState :: Word16 -> IO GlobalState
newGlobalState port = do
  uuid <- nextRandom
  let peerId = hash $ toASCIIBytes uuid
  torrents <- newTVarIO Seq.empty
  return $ GlobalState peerId port torrents

-- | Create a 'TorrentState'.
newTorrentState :: FilePath -- ^ Output directory for downloaded files
               -> MetaInfo
               -> IO (TorrentState 'Production)
newTorrentState dir meta = do
  let numPieces :: Integral a => a
      numPieces = fromIntegral (B.length $ pieces $ info meta) `quot` 20
  requestable_pieces <- newTVarIO $ IntSet.fromList [0..(numPieces-1)]
  bit_field <- newTVarIO $ BF.newBitField numPieces
  handles <- openHandles dir meta
  mvar <- newMVar ()
  sharedMessages <- newChan
  dp <- atomically $ DP.new numPieces
  return $ TorrentState meta bit_field requestable_pieces dp handles mvar sharedMessages

addActiveTorrent :: GlobalState -> TorrentState 'Production -> IO ()
addActiveTorrent global local =
  atomically $ modifyTVar' (globalStateTorrents global) (Seq.|> local)

openHandles :: FilePath -> MetaInfo -> IO (Seq (Word64, Word64, Handle))
openHandles dir meta = foldM opener (0, []) (files (info meta)) >>= return . Seq.fromList . reverse . snd
  where opener :: (Word64, [(Word64, Word64, Handle)]) -> FileInfo -> IO (Word64, [(Word64, Word64, Handle)])
        opener (offset, list) (FileInfo l n) = do
          hdl <- openFile (dir </> BC.unpack n) ReadWriteMode
          return (offset + l, (offset, offset + l, hdl) : list)

-- | Listen for connections.
-- Creates a new thread that accepts connections and spawns peer loops
-- with them.
btListen :: GlobalState -> IO (Async ())
btListen globalState = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral $ globalStateListenPort globalState) 0)
  listen sock 1
  async $ forever $ do
    (sock', addr) <- accept sock
    putStrLn "peer connected"
    forkIO $ startFromPeerHandshake globalState sock' addr

startFromPeerHandshake :: GlobalState -> Socket -> SockAddr -> IO ()
startFromPeerHandshake globalState sock addr = do
  handle <- socketToHandle sock ReadWriteMode
  handshake <- readHandshake handle

  case handshake of
    Just (BHandshake hisInfoHash peer) -> do
      ourTorrentState <- atomically $ do
        torrents <- readTVar (globalStateTorrents globalState)
        return $ find ((==hisInfoHash) . infoHash . torrentStateMetaInfo) torrents

      case ourTorrentState of
        Just torrent -> do
          writeHandshake handle globalState torrent

          let bf = BF.newBitField (pieceCount torrent)
              pData = newPeer bf addr peer

          mainPeerLoop torrent pData handle
          pure ()
        Nothing ->  hClose handle
    Nothing -> hClose handle

pieceCount :: TorrentState 'Production -> Word32
pieceCount = fromIntegral . (`quot` 20) . B.length . pieces . info . torrentStateMetaInfo

-- | Reach out to peer located at the address and enter the peer loop.
reachOutToPeer :: GlobalState -> TorrentState 'Production -> SockAddr -> IO ()
reachOutToPeer globalState state addr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock addr
  handle <- socketToHandle sock ReadWriteMode

  writeHandshake handle globalState state
  handshake <- readHandshake handle

  case handshake of
    Just (BHandshake hisInfoHash hisId) -> do
      let ourInfoHash = infoHash $ torrentStateMetaInfo state
          bitField = BF.newBitField (pieceCount state)

      when (hisInfoHash == ourInfoHash) $ do
        let pData = newPeer bitField addr hisId
        mainPeerLoop state pData handle
        pure ()
    Nothing -> hClose handle

writeHandshake :: Handle -> GlobalState -> TorrentState 'Production -> IO ()
writeHandshake handle globalState state = BL.hPut handle handshake
  where handshake = encode $ BHandshake (infoHash . torrentStateMetaInfo $ state) (globalStatePeerId globalState)
{-# INLINABLE writeHandshake #-}

readHandshake :: Handle -> IO (Maybe BHandshake)
readHandshake handle = do
  input <- BL.hGet handle 68
  case decodeOrFail input of
    Left _ -> pure Nothing -- the handshake is wrong/unsupported
    Right (_, _, handshake) -> pure $ Just handshake
{-# INLINABLE readHandshake #-}

mainPeerLoop :: TorrentState 'Production -> PeerData -> Handle -> IO (Either PeerError ())
mainPeerLoop state pData handle =
  runPeerMonad state pData handle entryPoint

-- | Ask the tracker for peers and return the result addresses.
queryTracker :: GlobalState -> TorrentState 'Production -> IO [SockAddr]
queryTracker globalState state = do
  let meta = torrentStateMetaInfo state
      url = fromByteString (announce meta) >>= parseUrl
      req = setQueryString [ ("peer_id", Just (globalStatePeerId globalState))
                           , ("info_hash", Just (infoHash meta))
                           , ("compact", Just "1")
                           , ("port", Just (BC.pack $ show $ globalStateListenPort globalState))
                           , ("uploaded", Just "0")
                           , ("downloaded", Just "0")
                           , ("left", Just (BC.pack $ show $ sum (Meta.length <$> Meta.files (info meta))))
                           ] (fromJust url)

  manager <- newManager defaultManagerSettings
  response <- httpLbs req manager
  let body = BL.toStrict $ responseBody response
  case AC.parseOnly value body of
    Right v ->
      return $ getPeers $ BL.fromStrict $ v ^. (bkey "peers" . bstring)
    _ -> return []

getPeers :: BL.ByteString -> [SockAddr]
getPeers src | BL.null src = []
getPeers src = SockAddrInet port ip : getPeers (BL.drop 6 src)
               where chunk = BL.take 6 src
                     ipRaw = BL.take 4 chunk
                     ip = runGet getWord32le ipRaw -- source is actually network order,
                                                   -- but HostAddress is too and Data.Binary
                                                   -- converts in `runGet`
                                                   -- we're avoiding this conversion
                     portSlice = BL.drop 4 chunk
                     port = fromIntegral (decode portSlice :: Word16)
{-# INLINABLE getPeers #-}
