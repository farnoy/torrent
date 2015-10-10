{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.Async
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
      clientState <- newClientState "." meta 8035
      void $ btListen clientState
      peers <- queryTracker clientState
      promises <- traverse (async . reachOutToPeer clientState) peers
      traverse_ waitCatch promises
      return ()
    Nothing -> Prelude.putStrLn "no files provided"
