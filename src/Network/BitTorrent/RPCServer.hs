{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Hosts an RPC server.
module Network.BitTorrent.RPCServer (
  server
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson((.=), object, ToJSON(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Conversion (fromByteString)
import Data.Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Word
import Flow
import qualified Network.BitTorrent.BitField as BF
import qualified Network.BitTorrent.Client as Client
import qualified Network.BitTorrent.LinkSpeed as LS
import qualified Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.Types
import Web.Scotty

data TorrentInfo = TorrentInfo
  { torrentInfoHash :: T.Text
  , torrentInfoName :: T.Text
  , torrentInfoProgress :: Float
  , torrentInfoStatus :: TorrentStatus
  , torrentInfoConnectedPeers :: Word32
  , torrentInfoBitField :: [Word8]
  , torrentInfoDownloadSpeed :: [Word64]
  }

statusToString :: TorrentStatus -> T.Text
statusToString Active = "active"
statusToString Paused = "paused"
statusToString Stopped = "stopped"

instance ToJSON TorrentInfo where
  toJSON info@TorrentInfo {} =
    object [ "infoHash" .= torrentInfoHash info
           , "name" .= torrentInfoName info
           , "progress" .= torrentInfoProgress info
           , "status" .= statusToString (torrentInfoStatus info)
           , "peers" .= torrentInfoConnectedPeers info
           , "bitField" .= torrentInfoBitField info
           , "downloadSpeed" .= torrentInfoDownloadSpeed info
           ]

extractTorrentInfo :: TorrentState -> IO TorrentInfo
extractTorrentInfo state = do
  (bf, status, peerCount, dlSpeed) <- atomically $ do
    bf <- readTVar $ torrentStateBitField state
    status <- readTVar $ torrentStateStatus state
    peerCount <- fromIntegral . Seq.length <$> (readTVar $ torrentStatePeerThreads state)
    dlSpeed <- readTVar (torrentStateDownloadSpeed state)
    return (bf, status, peerCount, dlSpeed)

  let meta = torrentStateMetaInfo state
      Just infoHash = fromByteString $ B16.encode $ Meta.infoHash meta
      Just fileName = fromByteString $ Meta.name $ head $ Meta.files $ Meta.info $ torrentStateMetaInfo state
      dlSpeedHistory = LS.recent 60 dlSpeed |> toList

  return $ TorrentInfo infoHash fileName (BF.completed bf) status peerCount (B.unpack $ BF.raw bf) dlSpeedHistory

server :: GlobalState -> ScottyM ()
server globalState = do
  get "/" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    let Just peerId = fromByteString $ B16.encode $ globalStatePeerId globalState
    text peerId
  get "/active" $ do
    response <- liftIO $ do
      torrents <- atomically $ readTVar $ globalStateTorrents globalState
      traverse extractTorrentInfo torrents
    setHeader "Access-Control-Allow-Origin" "*"
    json response

  post "/stop" $ do
    infoHashHex <- param "infoHash"
    let (infoHash, _) = B16.decode infoHashHex
    torrentState <- liftIO $ atomically $ do
      torrents <- readTVar $ globalStateTorrents globalState
      let torrentState =
            case Seq.findIndexL ((==infoHash) . Meta.infoHash . torrentStateMetaInfo) torrents of
              Just ix -> Just $ Seq.index torrents ix
              Nothing -> Nothing
      return torrentState

    setHeader "Access-Control-Allow-Origin" "*"

    case torrentState of
      Just s -> do
        liftIO $ Client.stopTorrent globalState s
        text "{\"status\": \"ok\"}"
      Nothing -> do
        text "{\"status\": \"failed\"}"

  post "/start" $ do
    infoHashHex <- param "infoHash"
    let (infoHash, _) = B16.decode infoHashHex
    torrentState <- liftIO $ atomically $ do
      torrents <- readTVar $ globalStateTorrents globalState
      let torrentState =
            case Seq.findIndexL ((==infoHash) . Meta.infoHash . torrentStateMetaInfo) torrents of
              Just ix -> Just $ Seq.index torrents ix
              Nothing -> Nothing
      return torrentState

    setHeader "Access-Control-Allow-Origin" "*"

    case torrentState of
      Just s -> do
        liftIO $ Client.startTorrent globalState s
        text "{\"status\": \"ok\"}"
      Nothing -> do
        text "{\"status\": \"failed\"}"
