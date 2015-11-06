-- | Provides a disk access layer.
-- Operations are threadsafe with explicit mutex locking.
module Network.BitTorrent.FileWriter (
  Network.BitTorrent.FileWriter.read
, write) where

import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import System.IO

-- | Reads from the handle.
read :: Handle
     -> MVar () -- ^ lock
     -> Word32 -- ^ offset in bytes
     -> Word32 -- ^ number of bytes to read
     -> IO ByteString
read hdl mvar offset size = withMVar mvar (const go)
  where go = do
          hSeek hdl AbsoluteSeek (fromIntegral offset)
          B.hGet hdl (fromIntegral size)
{-# INLINABLE read #-}

-- | Writes to the handle.
write :: Handle
      -> MVar () -- ^ lock
      -> Word32 -- ^ offset in bytes
      -> ByteString -- ^ data to write
      -> IO ()
write hdl mvar offset block = withMVar mvar (const go)
  where go = do
          hSeek hdl AbsoluteSeek (fromIntegral offset)
          B.hPut hdl block
{-# INLINABLE write #-}
