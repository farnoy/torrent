-- | ChunkFields are used to track download progress within a
-- single piece.
--
-- There are three states for any chunk:
--
--   * missing
--   * requested
--   * completed
--
-- This is enough information to coordinate download of a piece.
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

-- | Stores status of chunks inside a single piece.
data ChunkField = ChunkField
  { missingChunks :: IntSet -- ^ Missing chunks
  , requestedChunks :: IntSet -- ^ Requested chunks
  , completedChunks :: IntSet -- ^ Completed chunks
  } deriving(Eq, Show)

-- | Creates a new 'ChunkField' with specified length and all
-- chunks marked as missing.
newChunkField :: Int -> ChunkField
newChunkField n = ChunkField (IntSet.fromList [0..(n-1)])
                              IntSet.empty
                              IntSet.empty
{-# INLINABLE newChunkField #-}

-- | Gets the next missing chunk and marks it as requested.
--
-- If there is a missing chunk, returns a new
-- 'ChunkField' and its ID.
getNextChunk :: ChunkField
             -> Maybe (ChunkField, Int)
getNextChunk cf@(ChunkField missing requested _) =
  case IntSet.lookupGE 0 missing of
    Just k -> Just (cf { missingChunks = IntSet.difference missing (IntSet.singleton k)
                       , requestedChunks = IntSet.insert k requested
                       }
                   , k)
    Nothing -> Nothing
{-# INLINABLE getNextChunk #-}

-- | Mark the chunk as completed.
markCompleted :: ChunkField -> Int -> ChunkField
markCompleted cf@(ChunkField missing requested completed) ix =
  cf { requestedChunks = IntSet.difference requested (IntSet.singleton ix)
     , missingChunks = IntSet.difference missing (IntSet.singleton ix)
     , completedChunks = IntSet.insert ix completed }
{-# INLINABLE markCompleted #-}

-- | Mark the chunk as requested.
markRequested :: ChunkField -> Int -> ChunkField
markRequested cf@(ChunkField missing requested completed) ix =
  cf { missingChunks = IntSet.difference missing (IntSet.singleton ix)
     , completedChunks = IntSet.difference completed (IntSet.singleton ix)
     , requestedChunks = IntSet.insert ix requested }
{-# INLINABLE markRequested #-}

-- | Mark the chunk as missing.
markMissing :: ChunkField -> Int -> ChunkField
markMissing cf@(ChunkField missing requested _) ix =
  cf { missingChunks = IntSet.insert ix missing
     , requestedChunks = IntSet.difference requested (IntSet.singleton ix) }
{-# INLINABLE markMissing #-}

-- | /O(1)/ Check if the 'ChunkField' is completed.
-- To be completed, it cannot have any missing or requested pieces.
isCompleted :: ChunkField -> Bool
isCompleted (ChunkField missing requested _) =
  IntSet.null missing && IntSet.null requested
{-# INLINABLE isCompleted #-}

-- | /O(1)/ Check if the 'ChunkField' is requested.
-- To be requested, it cannot have any missing pieces.
isRequested :: ChunkField -> Bool
isRequested (ChunkField missing _ _) = IntSet.null missing
{-# INLINABLE isRequested #-}
