module Network.BitTorrent.PeerSelection (
  getNextPiece
, AvailabilityData
, addToAvailability
, removeFromAvailability
, getIncompletePieces
) where

import Data.Word
import Data.Vector.Storable ((//), (!))
import qualified Data.Vector.Storable as VS
import Network.BitTorrent.BitField (BitField)
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.Utility

type AvailabilityData = VS.Vector Word32

addToAvailability :: BitField -> AvailabilityData -> AvailabilityData
addToAvailability bf av = av // (f <$> [0..fromIntegral $ BF.length bf - 1])
  where f n = (fromIntegral n, (av ! n) + boolToWord (BF.get bf (fromIntegral n)))
{-# INLINABLE addToAvailability #-}

removeFromAvailability :: BitField -> AvailabilityData -> AvailabilityData
removeFromAvailability bf av = av // (f <$> [0..fromIntegral $ BF.length bf - 1])
  where f n = (fromIntegral n, (av ! n) - boolToWord (BF.get bf (fromIntegral n)))
{-# INLINABLE removeFromAvailability #-}

getNextPiece :: BitField -> AvailabilityData -> Maybe Word32
getNextPiece bf av = fromIntegral . fst <$> g
  where g = VS.ifoldl' (\counter index availability -> if BF.get bf (fromIntegral index)
            then counter
            else case counter of
              orig@(Just (_, a)) -> if availability > a
                               then Just (index, availability)
                               else orig
              Nothing -> Just (index, availability)
            ) Nothing av
{-# INLINABLE getNextPiece #-}

getIncompletePieces bf = filter (not . BF.get bf) [0..BF.length bf - 1]
{-# INLINABLE getIncompletePieces #-}
