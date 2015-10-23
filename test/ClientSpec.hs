{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module ClientSpec where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Exception.Base
import Control.Monad.STM
import Crypto.Hash.SHA1
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Language.Literals.Binary
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Client
import qualified Network.BitTorrent.FileWriter as FW
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.PWP
import Network.Socket
import SpecHelper
import System.Directory
import System.FilePath
import System.IO
import System.Random
import System.Timeout
import Test.Hspec

withSetup f = do
  tmpdir <- getTemporaryDirectory
  salt <- randomIO :: IO Word16
  let outDir = tmpdir </> show salt
  createDirectory outDir
  bracket (do
    generated <- getStdGen >>= pure . randomRs (6882 :: Word16, 15000)
    let ports = fromIntegral <$> generated
        bf = BF.newBitField pieceCount
        peer = newPeer bf (testAddr [1, 0, 0, 127] $ ports !! 0) "12345678901234567890"
        peer2 = newPeer bf (testAddr [1, 0, 0, 127] $ ports !! 1) "98765432109876543210"
    state <- newClientState outDir testMeta (fromIntegral $ ports !! 2)
    return (peer, peer2, state, testMeta)) (\_ -> do
      removeDirectoryRecursive outDir
    )
    f

data TestMessage = ReadHandshake BHandshake | ReadPWP PWP | WriteHandshake BHandshake | WritePWP PWP deriving(Show)

setupSocket :: PeerData -> [TestMessage] -> IO (Async Bool)
setupSocket peer messages = do
  let SockAddrInet port _ = address peer
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock NoDelay 1 -- Helps greatly on localhost
  bind sock (address peer)
  listen sock 10
  async $ do
    (sock', addr) <- accept sock
    handle <- socketToHandle sock' ReadWriteMode
    let evaluator previous [] = do
          return True
        evaluator previous all@(ReadHandshake expected : xs) = do
          input <- B.hGetSome handle (1024*1024)
          let parsed = runGetIncremental get `pushChunk` previous `pushChunk` input
          case parsed of
            Partial _ -> do
              evaluator (previous <> input) all
            Done unused _ handshake | handshake == expected -> do
              evaluator unused xs
            Done unused _ msg -> do
              putStrLn $ show msg <> " DOES NOT MATCH " <> show expected
              return False
            _ -> return False
        evaluator previous all@(WriteHandshake handshake : xs) = do
          BL.hPut handle $ encode handshake
          evaluator previous xs
        evaluator previous all@(WritePWP msg : xs) = do
          BL.hPut handle $ encode msg
          evaluator previous xs
        evaluator previous all@(ReadPWP expected : xs) = do
          input <- B.hGetSome handle (1024*1024)
          let parsed = runGetIncremental get `pushChunk` previous `pushChunk` input
          case parsed of
            Partial _ -> do
              evaluator (previous <> input) all
            Done unused _ msg | msg == expected -> do
              evaluator unused xs
            Done unused _ msg -> do
              putStrLn $ "EXPECTED " <> show expected <> " BUT GOT " <> show msg
              return False
            _ -> return False
    evaluator "" messages


spec :: SpecWith ()
spec = do
{-
  it "updates the peer's bitfield" $ withSetup $ \(peer, peer2, state, meta) -> do
    let someData = BL.toStrict $ BL.take pieceCount $ BL.cycle $ BL.pack $
                   fromIntegral <$> [readBinary "11011010", readBinary "00101000"]
    let bitField = BF.BitField someData pieceCount
    promise <- setupSocket peer
            [ ReadHandshake $ BHandshake (infoHash meta) $ myPeerId state
            , WriteHandshake $ BHandshake (infoHash meta) "12345678901234567890"
            , WritePWP $ Bitfield someData
            ]

    forkIO $ reachOutToPeer state (address peer)

    wait promise
    res <- timeout 10000 $ atomically $ do
      map <- readTVar (statePeers state)
      case Map.lookup (peerId peer) map of
        Just res | peerBitField res == bitField -> return True
        _ -> retry
    r <- atomically $ do
          map <- readTVar (statePeers state)
          return $ peerBitField <$> Map.lookup (peerId peer) map
    print r
    print bitField
    print pieceCount

    res `shouldBe` Just True
    -}

  it "handshakes properly" $ withSetup $ \(peer, peer2, state, meta) -> do
    promise <- setupSocket peer
            [ WriteHandshake $ BHandshake (infoHash meta) "12345678901234567890"
            , ReadHandshake $ BHandshake (infoHash meta) $ myPeerId state
            ]
    forkIO $ reachOutToPeer state (address peer)
    result <- wait promise
    result `shouldBe` True

  {-
  it "handles HAVE messages properly" $ withSetup $ \(peer, peer2, state, meta) -> do
    let newBitField = BF.set fullBitField 7 False
    promise <- setupSocket peer
            [ WriteHandshake $ BHandshake (infoHash meta) "12345678901234567890"
            , ReadHandshake $ BHandshake (infoHash meta) $ myPeerId state
            , WritePWP $ Bitfield $ BF.raw fullBitField
            , WritePWP $ Have 7
            ]

    forkIO $ reachOutToPeer state (address peer)

    res1 <- timeout 10000 $ wait promise
    res2 <- timeout 10000 $ atomically $ do
      peers <- readTVar (statePeers state)
      case Map.lookup "12345678901234567890" peers of
        Just p | BF.get (peerBitField p) 7 -> return True
        _ -> retry
    (res1, res2) `shouldBe` (Just True, Just True)
  -}


  it "can seed itself" $
    withSetup $ \(_, _, state, _) -> do
      withSetup $ \(_, _, state2, _) -> do
        bracket (do
          tmpdir <- getTemporaryDirectory
          openTempFile tmpdir "torrent") (\(path, handle) -> do
          hClose handle
          removeFile path) (\(path, handle) -> do
          B.hPut handle testData
          atomically $
            writeTVar (bitField state) fullBitField

          let state' = state { outputHandle = handle }
              addr = testAddr [1, 0, 0, 127] $ fromIntegral $ ourPort state'
          void $ btListen state'

          promise <- async $ reachOutToPeer state2 addr

          res <- timeout 1000000 $ atomically $ do
            bitField <- readTVar (bitField state2)
            if bitField == fullBitField
              then return True
              else retry
          r <- atomically $ readTVar (bitField state2)
          print r
          print fullBitField
          res `shouldBe` Just True)


