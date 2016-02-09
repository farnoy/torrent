{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Monad.Free.Church
import Control.Monad.STM
import Crypto.Hash.SHA1
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Int
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

testAddr bytes port = SockAddrInet port (decode $ BL.pack bytes :: Word32)

setupFullDownload pieceCount chunkCount = do
  let pieceSize :: Int
      pieceSize = 2^14 * chunkCount
      testData = BL.toStrict $ BL.take (fromIntegral pieceSize * pieceCount) $ BL.cycle "kopa to dopa"
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
  return $ (testData, pieceCount, pieceSize, testMeta)

fullDownload picker (testData, pieceCount, pieceSize, testMeta) = do
  tmpdir <- getTemporaryDirectory
  clientState <- newClientState tmpdir testMeta 8000
  outHandle <- openFile "/dev/null" WriteMode
  removeFile $ tmpdir <> "/output-test"
  let peerId = "12345678901234567890"
      addr = testAddr [1, 0, 0, 127] 8750
      peerData = newPeer (BF.negate $ BF.newBitField $ fromIntegral pieceCount) addr peerId
      action = do
        operation <- runMemory $ picker peerData testMeta
        case operation of
          Just (RequestChunk piece chunk (Request pieceId offset len)) ->
            let dataToSend = B.take (fromIntegral len)
                           $ B.drop (fromIntegral $ offset + pieceId * fromIntegral pieceSize)
                             testData
            in receiveChunk piece offset dataToSend >> action
          _ -> return ()
  runPeerMonad clientState peerData outHandle action
  return ()

main :: IO ()
main = defaultMain [
    bgroup "current algorithm, variable piece size" [
      env (setupFullDownload 64  16 ) (\e -> bench "64x16"  $ whnfIO $ fullDownload nextRequestOperation e)
    , env (setupFullDownload 64  64 ) (\e -> bench "64x64"  $ whnfIO $ fullDownload nextRequestOperation e)
    , env (setupFullDownload 64  128) (\e -> bench "64x128" $ whnfIO $ fullDownload nextRequestOperation e)
    , env (setupFullDownload 64  256) (\e -> bench "64x256" $ whnfIO $ fullDownload nextRequestOperation e)
    ]

  , bgroup "current algorithm, variable piece count" [
      env (setupFullDownload 16  64 ) (\e -> bench "16x64"  $ whnfIO $ fullDownload nextRequestOperation e)
    , env (setupFullDownload 64  64 ) (\e -> bench "64x64"  $ whnfIO $ fullDownload nextRequestOperation e)
    , env (setupFullDownload 128 64 ) (\e -> bench "128x64" $ whnfIO $ fullDownload nextRequestOperation e)
    , env (setupFullDownload 256 64 ) (\e -> bench "256x64" $ whnfIO $ fullDownload nextRequestOperation e)
  ]
  ]
