{-# OPTIONS_HADDOCK hide #-}
module Network.BitTorrent.Utility (
  divideSize
, boolToWord
, PieceId(..)
, ChunkId(..)
) where

import Data.Word

divideSize :: Integral a => a -> a -> a
divideSize a b | a `rem` b > 0 = (a `quot` b) + 1
divideSize a b = a `quot` b
{-# INLINABLE divideSize #-}

boolToWord :: Integral a => Bool -> a
boolToWord True = 1
boolToWord False = 0
{-# INLINABLE boolToWord #-}

-- | Holds the piece id.
newtype PieceId = PieceId Word32 deriving(Eq, Show, Ord)

-- | Holds the chunk id.
newtype ChunkId = ChunkId Word32 deriving(Eq, Show, Ord)
