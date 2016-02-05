-- | Provides a disk access layer.
-- Operations are threadsafe with explicit mutex locking.
module Network.BitTorrent.FileWriter (
  Network.BitTorrent.FileWriter.read
, write) where

import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (fold, traverse_)
import Data.Sequence (Seq)
import Data.Word
import Network.BitTorrent.Utility
import System.IO

-- | Reads from the handle.
read :: Seq (Word32, Word32, Handle)
     -> MVar () -- ^ lock
     -> Word32  -- ^ offset in bytes
     -> Word32  -- ^ number of bytes to read
     -> IO ByteString
read hdls mvar offset size = withMVar mvar (const go)
  where go = traverse read overlapping >>= return . fold
        read :: (Word32, Word32, Word32, Handle) -> IO ByteString
        read (base, lo, hi, hdl) = do
          hSeek hdl AbsoluteSeek (fromIntegral $ lo - base)
          B.hGet hdl (fromIntegral $ min (hi - lo) size)
        overlapping = fileOverlap addedBase offset (offset + size)
        addedBase = (\(lo, hi, hdl) -> (lo, lo, hi, hdl)) <$> hdls
{-# INLINABLE read #-}

-- | Writes to the handle.
write :: Seq (Word32, Word32, Handle)
      -> MVar ()    -- ^ lock
      -> Word32     -- ^ offset in bytes
      -> ByteString -- ^ data to write
      -> IO ()
write hdls mvar offset block = withMVar mvar (const go)
  where go = traverse_ write overlapping
        write :: (Word32, Word32, Word32, Handle) -> IO ()
        write (base, lo, hi, hdl) = do
          hSeek hdl AbsoluteSeek (fromIntegral $ lo - base)
          B.hPut hdl
            $ B.take (min (fromIntegral $ hi - lo) (B.length block))
            $ B.drop (fromIntegral $ lo - offset)
              block
        overlapping = fileOverlap addedBase offset (offset + fromIntegral (B.length block))
        addedBase = (\(lo, hi, hdl) -> (lo, lo, hi, hdl)) <$> hdls
{-# INLINABLE write #-}
