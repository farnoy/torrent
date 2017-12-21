-- | Provides a disk access layer.
-- Operations are threadsafe with explicit mutex locking.
module Network.BitTorrent.FileWriter (
  Network.BitTorrent.FileWriter.read
, write
, vectors) where

import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (fold, traverse_)
import Data.Sequence (Seq, ViewR(..), ViewL(..), viewr, viewl, takeWhileL, dropWhileL, (<|), (|>))
import Data.Word
import Foreign
import Network.BitTorrent.MemoryMap
import Network.BitTorrent.Utility
import System.IO

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

-- | Finds overlaps of any range with given array.
fileOverlap :: Seq (Word64, Word64, Word64, a) -- ^ Left-inclusive ranges to look in + user data
            -> Word64                  -- ^ lower bound to lookup
            -> Word64                  -- ^ upper bound to lookup
            -> Seq (Word64, Word64, Word64, a)
fileOverlap ranges lo hi = rightAdjusted
  where leftDropped = dropWhileL (\(_, _, h, _) -> h <= lo) ranges
        leftAdjusted = case viewl leftDropped of
          (base, leftLo, leftHi, leftC) :< leftDropped -> (base, max leftLo lo, leftHi, leftC) <| leftDropped
          _ -> leftDropped

        rightDropped = takeWhileL (\(_, l, _, _) -> hi >= l) leftAdjusted
        rightAdjusted = case viewr rightDropped of
          rightDropped :> (base, rightLo, rightHi, rightC) -> rightDropped |> (base, rightLo, min rightHi hi, rightC)
          EmptyR -> rightDropped

vectors :: Seq (Word64, Word64, MemoryMap)
        -> Word64 -- ^ lower bound in bytes
        -> Word64 -- ^ upper bound in bytes
        -> Seq (Ptr Word8, Word64) -- ^ vectors of pairs (base ptr, len)
vectors ranges lo hi = fmap preparePtr overlapping
  where
    addedBase = (\(lo, hi, mmap) -> (lo, lo, hi, mmap)) <$> ranges
    overlapping = fileOverlap addedBase lo hi
    preparePtr :: (Word64, Word64, Word64, MemoryMap) -> (Ptr Word8, Word64)
    preparePtr (_, lo, hi, mmap) = (getPtr mmap `plusPtr` (fromIntegral lo), hi)