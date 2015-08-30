module Network.BitTorrent.FileWriter (
  Network.BitTorrent.FileWriter.read
, write) where

import Control.Concurrent
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import System.IO

read :: Handle -> MVar () -> Word32 -> Word32 -> IO ByteString
read hdl mvar offset size = go `finally` putMVar mvar ()
  where go = do
          takeMVar mvar
          hSeek hdl AbsoluteSeek (fromIntegral offset)
          B.hGet hdl (fromIntegral size)
{-# INLINABLE read #-}

write :: Handle -> MVar () -> Word32 -> ByteString -> IO ()
write hdl mvar offset block = go `finally` putMVar mvar ()
  where go = do
          takeMVar mvar
          hSeek hdl AbsoluteSeek (fromIntegral offset)
          B.hPut hdl block
{-# INLINABLE write #-}
