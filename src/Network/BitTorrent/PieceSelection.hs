-- | Implements a rare-first algorithm for piece selection.
module Network.BitTorrent.PieceSelection (
  AvailabilityData
, getNextPiece
, addToAvailability
, removeFromAvailability
, getIncompletePieces
) where

import Data.Word
import Data.Vector.Unboxed ((//), (!))
import qualified Data.Vector.Unboxed as VU
import Network.BitTorrent.BitField (BitField)
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.Utility

-- | Stores availability - the number of peers that have data - for each piece.
type AvailabilityData = VU.Vector Word32

-- | Adds a 'BitField' to availability cache.
addToAvailability :: BitField -> AvailabilityData -> AvailabilityData
addToAvailability bf av = av // (f <$> [0..fromIntegral $ BF.length bf - 1])
  where f n = (fromIntegral n, (av ! n) + boolToWord (BF.get bf (fromIntegral n)))
{-# INLINABLE addToAvailability #-}

-- | Removes a 'BitField' from availability cache.
removeFromAvailability :: BitField -> AvailabilityData -> AvailabilityData
removeFromAvailability bf av = av // (f <$> [0..fromIntegral $ BF.length bf - 1])
  where f n = (fromIntegral n, (av ! n) - boolToWord (BF.get bf (fromIntegral n)))
{-# INLINABLE removeFromAvailability #-}

-- | Gets the next incomplete piece according to our algorithm.
getNextPiece :: BitField -- ^ download progress
             -> AvailabilityData -- ^ availability
             -> Maybe PieceId
getNextPiece bf av = PieceId . fromIntegral . fst <$> g
  where g = VU.ifoldl' (\counter index availability -> if BF.get bf (fromIntegral index)
            then counter
            else case counter of
              orig@(Just (_, a)) -> if availability > a
                               then Just (index, availability)
                               else orig
              Nothing -> Just (index, availability)
            ) Nothing av

-- | Retrieves all incomplete pieces.
getIncompletePieces :: BitField -> [Word32]
getIncompletePieces bf = filter (not . BF.get bf) [0..BF.length bf - 1]
{-# INLINABLE getIncompletePieces #-}
