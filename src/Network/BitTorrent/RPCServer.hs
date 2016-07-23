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
import Data.ByteString.Conversion (fromByteString)
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
  { torrentInfoHash :: T.Text
  , torrentInfoName :: T.Text
  , torrentInfoProgress :: Float
  }

instance ToJSON TorrentInfo where
  toJSON (TorrentInfo hash name progress) = object ["infoHash" .= hash
                                                   , "name" .= name
                                                   , "progress" .= progress
                                                   ]

extractTorrentInfo :: TorrentState 'Production -> IO TorrentInfo
extractTorrentInfo state = do
  bf <- atomically $ readTVar $ torrentStateBitField state
  let meta = torrentStateMetaInfo state
      infoHash = T.pack $ foldr showHex "" $ B.unpack $ Meta.infoHash meta
      Just fileName = fromByteString $ Meta.name $ head $ Meta.files $ Meta.info $ torrentStateMetaInfo state
  return $ TorrentInfo infoHash fileName (BF.completed bf)

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

