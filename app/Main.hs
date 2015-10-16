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
      traverse_ waitCatch promises
      return ()
    Nothing -> Prelude.putStrLn "no files provided"

progressLogger :: ClientState -> IO ()
progressLogger state = forever $ do
  bf <- atomically $ readTVar $ bitField state
  print bf
  threadDelay 5000000
