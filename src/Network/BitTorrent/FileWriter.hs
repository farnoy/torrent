-- | Provides a disk access layer.
-- Operations are threadsafe with explicit mutex locking.
module Network.BitTorrent.FileWriter (
  Network.BitTorrent.FileWriter.read
, write
, writePipe
) where

import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (fold, traverse_)
import Data.Sequence (Seq)
import Data.Word
import Foreign.Ptr
import Network.BitTorrent.Types
import Network.BitTorrent.Utility
import System.IO
import System.Posix.IO.ByteString
import System.Posix.Types

-- | Reads from the handle.
read :: Seq (Word64, Word64, Handle)
     -> MVar () -- ^ lock
     -> Word64  -- ^ offset in bytes
     -> Word64  -- ^ number of bytes to read
     -> IO ByteString
read hdls mvar offset size = {-# SCC "FW.read" #-} withMVar mvar (const go)
  where go = fmap fold (traverse read overlapping)
        read :: (Word64, Word64, Word64, Handle) -> IO ByteString
        read (base, lo, hi, hdl) = do
          hSeek hdl AbsoluteSeek (fromIntegral $ lo - base)
          B.hGet hdl (fromIntegral $ min (hi - lo) size)
        overlapping = fileOverlap addedBase offset (offset + size)
        addedBase = (\(lo, hi, hdl) -> (lo, lo, hi, hdl)) <$> hdls
{-# INLINABLE read #-}

-- | Writes to the handle.
write :: Seq (Word64, Word64, Handle)
      -> MVar ()    -- ^ lock
      -> Word64     -- ^ offset in bytes
      -> ByteString -- ^ data to write
      -> IO ()
write hdls mvar offset block = {-# SCC "FW.write" #-} withMVar mvar (const go)
  where go = {-# SCC "go" #-} traverse_ write overlapping
        write :: (Word64, Word64, Word64, Handle) -> IO ()
        write (base, lo, hi, hdl) = do
          {-# SCC "hSeek" #-} hSeek hdl AbsoluteSeek (fromIntegral $ lo - base)
          {-# SCC "hPut" #-} B.hPut hdl
            $ B.take (min (fromIntegral $ hi - lo) (B.length block))
            $ B.drop (fromIntegral $ lo - offset)
              block
        overlapping = {-# SCC "overlapping" #-} fileOverlap addedBase offset (offset + fromIntegral (B.length block))
        addedBase = (\(lo, hi, hdl) -> (lo, lo, hi, hdl)) <$> hdls
{-# INLINABLE write #-}

-- | Writes to the handle.
writePipe :: Seq (Word64, Word64, Fd)
          -> MVar ()    -- ^ lock
          -> Word64     -- ^ offset in bytes
          -> Pipe       -- ^ data to write
          -> Word32     -- ^ length in bytes
          -> IO Bool
writePipe hdls mvar offset (Pipe src _) len = withMVar mvar (const go)
  where go = all id <$> traverse write overlapping
        write :: (Word64, Word64, Word64, Fd) -> IO Bool
        write (base, lo, hi, dest) = do
          fdSeek dest AbsoluteSeek (fromIntegral $ lo - base)
          spliceAll src nullPtr True dest nullPtr False (fromIntegral $ hi - lo) 3
        overlapping = fileOverlap addedBase offset (offset + fromIntegral len)
        addedBase = (\(lo, hi, hdl) -> (lo, lo, hi, hdl)) <$> hdls
{-# INLINABLE writePipe #-}
