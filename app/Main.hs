{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Client
import Network.BitTorrent.MetaInfo
import System.Environment

openTorrentFile :: String -> IO (Maybe MetaInfo)
openTorrentFile filename = do
  contents <- B.readFile filename
  return $ AC.maybeResult (AC.parse value contents) >>= parseMetaInfo

main :: IO ()
main = do
  args <- getArgs
  res <- openTorrentFile $ head args
  case res of
    Just meta -> do
      clientState <- newClientState "." meta 8035
      _ <- btListen clientState
      queryTracker clientState
      return ()
    Nothing -> Prelude.putStrLn "no files provided"
