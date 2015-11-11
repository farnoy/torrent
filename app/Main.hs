{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Network.BitTorrent.Bencoding
import Network.BitTorrent.ChunkField as CF
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.Types
import System.Environment
import System.Posix.Signals

openTorrentFile :: String -> IO (Maybe MetaInfo)
openTorrentFile filename = do
  contents <- BL.readFile filename
  return $ AL.maybeResult (AL.parse value contents) >>= parseMetaInfo

main :: IO ()
main = do
  args <- getArgs
  res <- openTorrentFile $ head args
  case res of
    Just meta -> do
      clientState <- newClientState "." meta globalPort
      void $ btListen clientState
      peers <- queryTracker clientState
      promises <- traverse (async . reachOutToPeer clientState) peers
      -- dont do this here in the future
      void $ forkIO $ progressLogger clientState
      void $ forkIO $ sharedMessagesLogger clientState
      void $ forkIO $ periodicCheckup clientState
      void $ installHandler sigINT (CatchOnce (traverse_ cancel promises)) Nothing
      waitOnPeers promises
      return ()
    Nothing -> Prelude.putStrLn "no files provided"

waitOnPeers :: [Async a] -> IO ()
waitOnPeers promises = do
  (finished, res) <- waitAnyCatch promises
  putStrLn "promise exited"
  case res of
    Left e -> putStrLn ("with error " ++ show e)
    Right _ -> putStrLn "success & quit"
  waitOnPeers (filter (/= finished) promises)

progressLogger :: ClientState -> IO ()
progressLogger state = forever $ do
  bf <- atomically $ readTVar $ bitField state
  print bf
  threadDelay 5000000

sharedMessagesLogger :: ClientState -> IO ()
sharedMessagesLogger state =
  forever $ readChan (sharedMessages state) >>= print

periodicCheckup :: ClientState -> IO ()
periodicCheckup state = forever $ do
  threadDelay 1000000
  writeChan (sharedMessages state) Checkup
  chunks <- atomically (readTVar (pieceChunks state))
  print (CF.requestedChunks . fst <$> chunks)
