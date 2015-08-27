module Network.BitTorrent.ChunkField (
  ChunkStatus(..)
, ChunkField
, newChunkField
, getNextChunk
, getIncompleteChunks
, markCompleted
, markRequested
, isCompleted
, isRequested
) where

import Data.Sequence as Seq
import Data.Foldable as Foldable

data ChunkStatus = Missing | Requested | Completed
                   deriving(Eq, Show)

type ChunkField = Seq ChunkStatus

newChunkField :: Integral a => a -> ChunkField
newChunkField n = Seq.replicate (fromIntegral n) Missing
{-# INLINABLE newChunkField #-}

getNextChunk :: Integral a => ChunkField -> Maybe (ChunkField, a)
getNextChunk cf = (\i -> (Seq.update i Requested cf, fromIntegral i)) <$> ix
  where ix = Seq.elemIndexL Missing cf
{-# INLINABLE getNextChunk #-}

getIncompleteChunks :: Integral a => ChunkField -> Maybe (ChunkField, [a])
getIncompleteChunks cf = Just (cf, Foldable.toList values)
  where values = fmap snd
               $ Seq.filter ((/=Completed) . fst)
               $ Seq.mapWithIndex (\i a -> (a, fromIntegral i)) cf
{-# INLINABLE getIncompleteChunks #-}

markCompleted :: Integral a => ChunkField -> a -> ChunkField
markCompleted cf ix = Seq.update (fromIntegral ix) Completed cf
{-# INLINABLE markCompleted #-}

markRequested :: Integral a => ChunkField -> a -> ChunkField
markRequested cf ix = Seq.update (fromIntegral ix) Requested cf
{-# INLINABLE markRequested #-}

isCompleted :: ChunkField -> Bool
isCompleted = Foldable.all (==Completed)
{-# INLINABLE isCompleted #-}

isRequested :: ChunkField -> Bool
isRequested = Foldable.all (/=Missing)
{-# INLINABLE isRequested #-}
