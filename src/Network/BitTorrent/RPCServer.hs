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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import qualified Network.BitTorrent.BitField as BF
import qualified Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.Types
import Network.BitTorrent.Utility
import Numeric
import Web.Scotty

data TorrentInfo = TorrentInfo
  { infoHash :: T.Text
  , progress :: Float
  } deriving (Generic, Aeson.ToJSON)

extractTorrentInfo :: TorrentState 'Production -> IO TorrentInfo
extractTorrentInfo state = do
  bf <- atomically $ readTVar $ torrentStateBitField state
  let infoHash = T.pack $ foldr showHex "" (B.unpack $ Meta.infoHash $ torrentStateMetaInfo state)
  return $ TorrentInfo infoHash (BF.completed bf)

server :: GlobalState -> ScottyM ()
server globalState = do
  get "/" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    let peerId = foldr showHex "" (B.unpack (globalStatePeerId globalState))
    text $ TL.pack peerId
  get "/active" $ do
    response <- liftIO $ do
      torrents <- atomically $ readTVar $ globalStateTorrents globalState
      traverse extractTorrentInfo torrents
    setHeader "Access-Control-Allow-Origin" "*"
    json response

