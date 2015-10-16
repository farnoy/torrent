{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
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
      forkIO $ progressLogger clientState
      forkIO $ sharedMessagesLogger clientState
      installHandler sigINT (CatchOnce (traverse_ cancel promises)) Nothing
      traverse_ closePromise promises
      return ()
    Nothing -> Prelude.putStrLn "no files provided"

closePromise p = do
  res <- waitCatch p
  putStrLn "promise exited"
  case res of
    Left e -> print e
    Right _ -> print "success & quit"

progressLogger :: ClientState -> IO ()
progressLogger state = forever $ do
  bf <- atomically $ readTVar $ bitField state
  print bf
  threadDelay 5000000

sharedMessagesLogger :: ClientState -> IO ()
sharedMessagesLogger state =
  forever $ readChan (sharedMessages state) >>= print
