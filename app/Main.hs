{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.Types
import Network.BitTorrent.Utility
import System.Environment
import System.IO
import System.Posix.Signals

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
  print torrents
  listener <- btListen globalState
  promises <- traverse (runTorrent globalState) (catMaybes torrents)
  void $ installHandler sigINT (CatchOnce (cleanup globalState listener promises)) Nothing
  traverse_ wait promises

runTorrent :: GlobalState -> MetaInfo -> IO (Async ())
runTorrent globalState meta = async $ do
  torrentState <- newTorrentState "." meta
  addActiveTorrent globalState torrentState
  peers <- queryTracker globalState torrentState
  promises <- traverse (async . reachOutToPeer globalState torrentState) peers
  void $ forkIO $ progressLogger torrentState
  void $ forkIO $ sharedMessagesLogger torrentState
  void $ forkIO $ periodicCheckup torrentState
  waitOnPeers promises
  return ()

cleanup :: GlobalState -> Async a -> [Async b]-> IO ()
cleanup globalState listener promises = do
  cancel listener
  torrents <- atomically $ readTVar $ globalStateTorrents globalState
  traverse_ (\state -> writeChan (torrentStateSharedMessages state) Exit) torrents
  putStrLn "waiting 5 seconds to kill all"
  threadDelay 5000000
  traverse_ (traverse_ (\(_, _, h) -> hClose h)) (torrentStateOutputHandles <$> torrents)
  traverse_ cancel promises

waitOnPeers :: [Async a] -> IO ()
waitOnPeers [] = return ()
waitOnPeers promises = do
  (finished, res) <- waitAnyCatch promises
  putStrLn "promise exited"
  case res of
    Left e -> putStrLn ("with error " ++ show e)
    Right _ -> putStrLn "success & quit"
  waitOnPeers (filter (/= finished) promises)

progressLogger :: TorrentState 'Production -> IO ()
progressLogger state = forever $ do
  bf <- atomically $ readTVar $ torrentStateBitField state
  print bf
  threadDelay 5000000

sharedMessagesLogger :: TorrentState 'Production -> IO ()
sharedMessagesLogger state =
  forever $ readChan (torrentStateSharedMessages state) >>= print

periodicCheckup :: TorrentState 'Production -> IO ()
periodicCheckup state = forever $ do
  threadDelay 1000000
  writeChan (torrentStateSharedMessages state) Checkup
  -- chunks <- atomically (readTVar (pieceChunks state))
  --print (CF.requestedChunks <$> chunks)
