{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Exception.Base
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
import qualified Network.BitTorrent.RPCServer as RPC
import Network.BitTorrent.Types
import System.Environment
import System.IO
import System.Posix.Signals
import Web.Scotty

openTorrentFile :: String -> IO (Maybe MetaInfo)
openTorrentFile filename = do
  contents <- BL.readFile filename
  return $ AL.maybeResult (AL.parse value contents) >>= parseMetaInfo

main :: IO ()
main = do
  args <- getArgs
  print args
  globalState <- newGlobalState 8035
  torrents <- traverse openTorrentFile args
  rpcServer <- async $ scotty 8036 (RPC.server globalState)
  listener <- btListen globalState
  print torrents
  traverse_ (forkIO . runTorrent globalState) (catMaybes torrents)
  void $ installHandler sigINT (CatchOnce (cancel listener *> cancel rpcServer)) Nothing
  void $ forkIO $ progressLogger globalState
  void $ forkIO $ periodicCheckup globalState
  (do wait listener
      wait rpcServer)
    `finally` cleanup globalState

cleanup :: GlobalState -> IO ()
cleanup globalState = do
  torrents <- atomically $ readTVar $ globalStateTorrents globalState
  traverse_ (stopTorrent globalState) torrents
  putStrLn "quiting cleanup"

progressLogger :: GlobalState -> IO ()
progressLogger globalState = forever $ do
  torrents <- atomically $ readTVar (globalStateTorrents globalState)
  let printBitField state = do
        bf <- atomically $ readTVar $ torrentStateBitField state
        print bf
  traverse_ printBitField torrents
  threadDelay 5000000

periodicCheckup :: GlobalState -> IO ()
periodicCheckup globalState = forever $ do
  threadDelay 1000000
  torrents <- atomically $ readTVar (globalStateTorrents globalState)
  traverse_ (flip writeChan Checkup . torrentStateSharedMessages) torrents
