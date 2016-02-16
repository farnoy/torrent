{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides operations for BitFields that are used in the protocol.
module Network.BitTorrent.BitField (
  BitField(..)
, newBitField
, get
, set
, completed
, toPWP
) where

import Control.DeepSeq
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable as Foldable
import qualified Data.ByteString as B
import Data.Monoid
import Data.Word
import GHC.Generics (Generic)
import Network.BitTorrent.PWP
import Network.BitTorrent.Utility

-- | Holds the completion status of pieces.
-- Works by using tightly packed bits to store this information efficiently.
-- Can describe 8 piece statuses per byte.
data BitField = BitField
  { raw :: !ByteString -- ^ Raw byte array.
  , length :: Word32 -- ^ Length of the bitfield.
  } deriving(Show, Eq, Generic, NFData)

-- | /O(n)/ Creates a new bitfield with the specified length.
-- Starts out with all pieces marked as unfinished.
newBitField :: Word32 -> BitField
newBitField len = BitField (B.replicate (fromIntegral byteLength) 0) len
  where byteLength = divideSize len 8
{-# INLINABLE newBitField #-}

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
