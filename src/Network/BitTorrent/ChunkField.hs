module Network.BitTorrent.ChunkField (
  ChunkField
, newChunkField
, getNextChunk
, markCompleted
, markRequested
, markMissing
, isCompleted
, isRequested
) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data ChunkField = ChunkField { missingChunks :: IntSet
                             , requestedChunks :: IntSet
                             , completedChunks :: IntSet
                             } deriving(Eq, Show)

newChunkField :: Int -> ChunkField
newChunkField n = ChunkField (IntSet.fromList [0..(n-1)])
                              IntSet.empty
                              IntSet.empty
{-# INLINABLE newChunkField #-}

getNextChunk :: ChunkField -> Maybe (ChunkField, Int)
getNextChunk cf@(ChunkField missing requested _) =
  case IntSet.lookupGE 0 missing of
    Just k -> Just (cf { missingChunks = IntSet.difference missing (IntSet.singleton k)
                       , requestedChunks = IntSet.insert k requested
                       }
                   , k)
    Nothing -> Nothing
{-# INLINABLE getNextChunk #-}

markCompleted :: ChunkField -> Int -> ChunkField
markCompleted cf@(ChunkField missing requested completed) ix =
  cf { requestedChunks = IntSet.difference requested (IntSet.singleton ix)
     , missingChunks = IntSet.difference missing (IntSet.singleton ix)
     , completedChunks = IntSet.insert ix completed }
{-# INLINABLE markCompleted #-}

markRequested :: ChunkField -> Int -> ChunkField
markRequested cf@(ChunkField missing requested completed) ix =
  cf { missingChunks = IntSet.difference missing (IntSet.singleton ix)
     , completedChunks = IntSet.difference completed (IntSet.singleton ix)
     , requestedChunks = IntSet.insert ix requested }
{-# INLINABLE markRequested #-}

markMissing :: ChunkField -> Int -> ChunkField
markMissing cf@(ChunkField missing requested _) ix =
  cf { missingChunks = IntSet.insert ix missing
     , requestedChunks = IntSet.difference requested (IntSet.singleton ix) }
{-# INLINABLE markMissing #-}

isCompleted :: ChunkField -> Bool
isCompleted (ChunkField missing requested _) =
  IntSet.null missing && IntSet.null requested
{-# INLINABLE isCompleted #-}

isRequested :: ChunkField -> Bool
isRequested (ChunkField missing _ _) = IntSet.null missing
{-# INLINABLE isRequested #-}
