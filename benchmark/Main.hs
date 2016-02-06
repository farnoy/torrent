{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.DeepSeq
import Control.Monad.Free.Church
import Control.Monad.STM
import Crypto.Hash.SHA1
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Word
import Network.BitTorrent.Bencoding
import Network.BitTorrent.BitField as BF
import Network.BitTorrent.Client
import Network.BitTorrent.MemoryMonad
import Network.BitTorrent.MetaInfo as Meta
import Network.BitTorrent.PeerMonad
import Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.PWP
import Network.BitTorrent.Types
import Network.Socket
import System.Directory
import System.IO

import Criterion.Main

pieceSize :: Integral a => a
pieceSize = 2^14 * 60 -- 60 chunks
testData = BL.toStrict $ BL.take (pieceSize * 48) $ BL.cycle "kopa to dopa"
pieceCount :: Integral a => a
pieceCount = ceiling $ fromIntegral (B.length testData) / (fromIntegral pieceSize)
hashes = go testData
  where go "" = ""
        go input = hash (B.take pieceSize input) <> go (B.drop pieceSize input)

infoDictionaryRaw =
  Dictionary $ Map.fromList [ ("piece length", Number $ fromIntegral pieceSize)
                            , ("pieces", String hashes)
                            , ("length", Number $ fromIntegral $ B.length testData)
                            , ("name", String $ BC.pack $ "output-test")
                            ]

metaInfoRaw =
  Dictionary $ Map.fromList [ ("info", infoDictionaryRaw)
                            , ("announce", String "http://tracker.archlinux.org:6969/announce")
                            ]

testMeta = fromJust $ parseMetaInfo metaInfoRaw

testAddr bytes port = SockAddrInet port (decode $ BL.pack bytes :: Word32)

data NFEnv = NFEnv ClientState PeerData Handle (F PeerMonad ())

instance NFData NFEnv where
  rnf _ = ()

setupFullDownload = do
  tmpdir <- getTemporaryDirectory
  clientState <- newClientState tmpdir testMeta 8000
  outHandle <- openFile "/dev/null" WriteMode
  removeFile $ tmpdir <> "/output-test"
  let peerId = "12345678901234567890"
      addr = testAddr [1, 0, 0, 127] 8750
      peerData = newPeer (BF.negate $ BF.newBitField pieceCount) addr peerId
      action = do
        operation <- runMemory $ nextRequestOperation peerData testMeta
        case operation of
          Just (RequestChunk piece chunk (Request pieceId offset len)) ->
            let dataToSend = B.take (fromIntegral len)
                           $ B.drop (fromIntegral $ offset + pieceId * pieceSize)
                             testData
            in receiveChunk piece offset dataToSend >> action
          _ -> return ()
  return $ NFEnv clientState peerData outHandle action

fullDownload (NFEnv clientState peerData outHandle action) =
  runPeerMonad clientState peerData outHandle action

main :: IO ()
main = defaultMain [
  bgroup "piece selection" [
    env setupFullDownload (\e -> bench "full" $ whnfIO $ fullDownload e) ]
  ]
