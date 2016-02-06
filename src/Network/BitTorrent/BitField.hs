{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides operations for BitFields that are used in the protocol.
module Network.BitTorrent.BitField (
  BitField(..)
, newBitField
, fromChunkFields
, get
, set
, completed
, toPWP
-- * Useful operations
, intersection
, difference
, union
, Network.BitTorrent.BitField.negate
) where

import Control.DeepSeq
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable as Foldable
import qualified Data.ByteString as B
import Data.Monoid
import Data.Word
import GHC.Generics (Generic)
import Network.BitTorrent.ChunkField as CF
import Network.BitTorrent.PWP
import Network.BitTorrent.Utility

-- | Holds the completion status of pieces.
-- Works by using tightly packed bits to store this information efficiently.
-- Can describe 8 piece statuses per byte.
data BitField = BitField
  { raw :: ByteString -- ^ Raw byte array.
  , length :: Word32 -- ^ Length of the bitfield.
  } deriving(Show, Eq, Generic, NFData)

-- | /O(n)/ Creates a new bitfield with the specified length.
-- Starts out with all pieces marked as unfinished.
newBitField :: Word32 -> BitField
newBitField len = BitField (B.replicate (fromIntegral byteLength) 0) len
  where byteLength = divideSize len 8
{-# INLINABLE newBitField #-}

-- | /O(n)/ Creates a bitfield out of multiple 'ChunkField's.
-- ChunkFields that satisfy 'CF.isRequested' are marked
-- as completed pieces in the resulting 'BitField'.
--
-- This is useful for figuring out which pieces are incomplete
-- but have been requested, particularly with `intersection`.
fromChunkFields :: Word32 -- ^ Length of the new BitField
                -> [(PieceId, ChunkField)] -- ^ Pairs of Piece ID and 'ChunkField'
                -> BitField
fromChunkFields len chunksOriginal =
  BitField (snd $ B.mapAccumL f (PieceId 0, chunksOriginal) fresh) len
  where byteLength = divideSize len 8
        fresh = B.replicate (fromIntegral byteLength) 0
        f (PieceId ix, chunks) w =
          let (chunks', res) = foldl' (g ix) (chunks, w) [0..7]
          in ((PieceId (ix + 8), chunks'), res)
        {-# INLINABLE f #-}
        g ix (chunks, byte) n = case chunks of
          (PieceId currentIx, cf):restChunks | currentIx == ix + n ->
            if CF.isRequested cf
              then (restChunks, byte `setBit` fromIntegral (7 - n))
              else (restChunks, byte)
          _ -> (chunks, byte)
        {-# INLINABLE g #-}


-- | /O(n)/ Returns the intersection of both bitfields.
intersection :: BitField -> BitField -> BitField
intersection (BitField a len) (BitField b _) = BitField (snd $ B.mapAccumL f 0 a) len
  where f ix w = (ix + 1, w .&. B.index b ix)
        {-# INLINABLE f #-}

-- | /O(n)/ Returns the union of both bitfields.
union :: BitField -> BitField -> BitField
union (BitField a len) (BitField b _) = BitField (snd $ B.mapAccumL f 0 a) len
  where f ix w = (ix + 1, w .|. B.index b ix)
        {-# INLINABLE f #-}

-- | /O(n)/ Returns the difference of bitfields. A \ B
difference :: BitField -> BitField -> BitField
difference (BitField a len) (BitField b _) = BitField (snd $ B.mapAccumL f 0 a) len
  where f ix w = (ix + 1, w `xor` (w .&. B.index b ix))
        {-# INLINABLE f #-}

-- | /O(n)/ Returns the bitfield with all values negated.
negate :: BitField -> BitField
negate (BitField a len) = BitField (B.map complement a) len

-- | /O(1)/ Get the status of a single piece.
get :: BitField -> Word32 -> Bool
get (BitField field _) ix = testBit word (7 - fromIntegral ix `rem` 8)
  where byteIx =  fromIntegral $ ix `quot` 8
        word =  B.index field byteIx
{-# INLINABLE get #-}

-- | /O(n)/ Sets the status of a single piece by returning a new
-- bitfield with the change applied.
set :: BitField
    -> Word32 -- ^ Piece ID
    -> Bool -- ^ New status
    -> BitField
set (BitField field len) ix val = (BitField $! updatedField) $! len
  where byteIx = fromIntegral ix `quot` 8
        word = B.index field byteIx
        modifier True = setBit
        modifier False = clearBit
        updatedByte = modifier val word (7 - fromIntegral ix `rem` 8)
        updatedField = B.take byteIx field <> B.singleton updatedByte <> B.drop (byteIx + 1) field
{-# INLINABLE set #-}

-- | /O(n)/ Get the ratio of completed pieces.
completed :: BitField
          -> Float -- ^ Result in range /[0;1]/
completed (BitField b len) = fromIntegral (Foldable.sum (popCount <$> B.unpack b)) / fromIntegral len
{-# INLINABLE completed #-}

-- | /O(1)/ Cast the BitField to a PWP message.
toPWP :: BitField -> PWP
toPWP (BitField raw _) = Bitfield raw
{-# INLINABLE toPWP #-}
