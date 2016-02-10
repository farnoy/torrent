{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}

-- | Provides atomic transactions on shared memory.
module Network.BitTorrent.MemoryMonad (
  MemoryMonad(..)

-- ops
, getChunks
, modifyChunks
, getBitfield
, modifyBitfield
-- , getAvailability
-- , modifyAvailability
, getRequestablePieces
, modifyRequestablePieces

-- run
, runMemoryMonadSTM
) where

import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Monad.Trans.Free.Church
import Control.Monad.STM
import Data.IntSet (IntSet)
import qualified Network.BitTorrent.BitField as BF
-- import Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.Types

-- | Encodes memory operations.
data MemoryMonad a = GetChunks (Chunks -> a)
                   | ModifyChunks (Chunks -> Chunks) a
                   | ReadBitfield (BF.BitField -> a)
                   | ModifyBitfield (BF.BitField -> BF.BitField) a
                   {-
                   | ReadAvailability (AvailabilityData -> a)
                   | ModifyAvailability (AvailabilityData -> AvailabilityData) a
                   -}
                   | ReadRequestablePieces (IntSet -> a)
                   | ModifyRequestablePieces (IntSet -> IntSet) a
                   deriving(Functor)

-- | Gets 'Chunks'.
getChunks :: F MemoryMonad Chunks
getChunks = liftF $ GetChunks id
{-# INLINABLE getChunks #-}

-- | Modifies 'Chunks'.
modifyChunks :: (Chunks -> Chunks) -> F MemoryMonad ()
modifyChunks mut = liftF $ ModifyChunks mut ()
{-# INLINABLE modifyChunks #-}

-- | Gets 'BF.BitField'.
getBitfield :: F MemoryMonad BF.BitField
getBitfield = liftF $ ReadBitfield id
{-# INLINABLE getBitfield #-}

-- | Modifies 'BF.BitField'.
modifyBitfield :: (BF.BitField -> BF.BitField) -> F MemoryMonad ()
modifyBitfield mut = liftF $ ModifyBitfield mut ()
{-# INLINABLE modifyBitfield #-}

{-

-- | Gets 'AvailabilityData'.
getAvailability :: F MemoryMonad AvailabilityData
getAvailability = liftF $ ReadAvailability id
{-# INLINABLE getAvailability #-}

-- | Modifies 'AvailabilityData'.
--
-- Always remember to subtract your previous contribution to
-- availability if applicable:
--
-- @
-- modifyAvailability $ addToAvailability newBf . removeFromAvailability oldBf
-- @
modifyAvailability :: (AvailabilityData -> AvailabilityData) -> F MemoryMonad ()
modifyAvailability mut = liftF $ ModifyAvailability mut ()
{-# INLINABLE modifyAvailability #-}
-}

-- | Gets requestable pieces.
getRequestablePieces :: F MemoryMonad IntSet
getRequestablePieces = liftF $ ReadRequestablePieces id
{-# INLINABLE getRequestablePieces #-}

-- | Modifies requestable pieces.
modifyRequestablePieces :: (IntSet -> IntSet) -> F MemoryMonad ()
modifyRequestablePieces mut = liftF $ ModifyRequestablePieces mut ()
{-# INLINABLE modifyRequestablePieces #-}

evalMemoryMonad :: ClientState -> MemoryMonad (STM a) -> STM a
evalMemoryMonad state (GetChunks next) = do
  chunks <- readTVar (pieceChunks state)
  next chunks
evalMemoryMonad state (ModifyChunks f next) = do
  modifyTVar' (pieceChunks state) (force . f)
  next
evalMemoryMonad state (ReadBitfield next) = do
  res <- readTVar (bitField state)
  next res
evalMemoryMonad state (ModifyBitfield mut next) = do
  modifyTVar' (bitField state) (force . mut)
  next
{-
evalMemoryMonad state (ReadAvailability next) = do
  res <- readTVar (availabilityData state)
  next res
evalMemoryMonad state (ModifyAvailability mut next) = do
  modifyTVar' (availabilityData state) (force . mut)
  next
-}
evalMemoryMonad state (ReadRequestablePieces next) = do
  res <- readTVar (requestablePieces state)
  next res
evalMemoryMonad state (ModifyRequestablePieces mut next) = do
  modifyTVar' (requestablePieces state) (force . mut)
  next

-- | Runs the whole transaction atomically under STM.
runMemoryMonadSTM :: ClientState -> F MemoryMonad a -> STM a
runMemoryMonadSTM state = iterM (evalMemoryMonad state)
