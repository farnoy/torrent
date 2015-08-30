{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.BitTorrent.FileWriter where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import System.IO

data Operation = ReadBlock Word32 Word32 (ByteString -> IO ())
               | WriteBlock Word32 ByteString

operate :: Handle -> IO (Chan Operation)
operate handle = do
  operations <- newChan
  _ <- forkIO $ forever $ readChan operations >>= perform handle
  return operations

perform :: Handle -> Operation -> IO ()
perform handle (ReadBlock offset size action) = do
  hSeek handle AbsoluteSeek (fromIntegral offset)
  block <- B.hGet handle (fromIntegral size)
  _ <- forkIO $ action block
  return ()
perform handle (WriteBlock offset block) = do
  hSeek handle AbsoluteSeek (fromIntegral offset)
  B.hPut handle block
