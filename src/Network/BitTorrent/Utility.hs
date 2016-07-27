{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Network.BitTorrent.Utility (
  divideSize
, boolToWord
, fileOverlap
, PieceId(..)
, ChunkId(..)
) where

import Control.DeepSeq
import Data.Sequence
import Data.Word
import Data.Hashable
import GHC.Generics (Generic)

divideSize :: Integral a => a -> a -> a
divideSize a b | a `rem` b > 0 = (a `quot` b) + 1
divideSize a b = a `quot` b
{-# INLINABLE divideSize #-}

boolToWord :: Integral a => Bool -> a
boolToWord True = 1
boolToWord False = 0
{-# INLINABLE boolToWord #-}

-- | Holds the piece id.
newtype PieceId = PieceId Word32 deriving(Eq, Show, Ord, Generic, NFData, Hashable)

-- | Holds the chunk id.
newtype ChunkId = ChunkId Word32 deriving(Eq, Show, Ord, Generic, NFData)

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

