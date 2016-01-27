{-# OPTIONS_HADDOCK hide #-}
module Network.BitTorrent.Utility (
  divideSize
, boolToWord
, fileOverlap
, PieceId(..)
, ChunkId(..)
) where

import Data.Sequence
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

-- | Finds overlaps of any range with given array.
fileOverlap :: Seq (Word32, Word32, Word32, a) -- ^ Left-inclusive ranges to look in + user data
            -> Word32                  -- ^ lower bound to lookup
            -> Word32                  -- ^ upper bound to lookup
            -> Seq (Word32, Word32, Word32, a)
fileOverlap ranges lo hi = rightAdjusted
  where leftDropped = dropWhileL (\(_, l, h, _) -> h <= lo) ranges
        leftAdjusted = case viewl leftDropped of
          (base, leftLo, leftHi, leftC) :< leftDropped -> (base, max leftLo lo, leftHi, leftC) <| leftDropped
          _ -> leftDropped

        rightDropped = takeWhileL (\(_, l, h, _) -> hi >= l) leftAdjusted
        rightAdjusted = case viewr rightDropped of
          rightDropped :> (base, rightLo, rightHi, rightC) -> rightDropped |> (base, rightLo, min rightHi hi, rightC)
          EmptyR -> rightDropped

