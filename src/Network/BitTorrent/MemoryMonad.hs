{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}

module Network.BitTorrent.MemoryMonad (
#ifdef TESTING
  MemoryMonad(..)
#else
  MemoryMonad()
#endif

-- ops
, getChunks
, modifyChunks
, getBitfield
, modifyBitfield
, getAvailability
, modifyAvailability

-- run
, runMemoryMonadSTM
) where

import Control.Concurrent.STM.TVar
import Control.Monad.Free.Church
import Control.Monad.STM
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.PeerSelection as PS
import Network.BitTorrent.Types

data MemoryMonad a = GetChunks (Chunks -> a)
                   | ModifyChunks (Chunks -> Chunks) a
                   | ReadBitfield (BF.BitField -> a)
                   | ModifyBitfield (BF.BitField -> BF.BitField) a
                   | ReadAvailability (AvailabilityData -> a)
                   | ModifyAvailability (AvailabilityData -> AvailabilityData) a
                   deriving(Functor)

getChunks :: F MemoryMonad Chunks
getChunks = liftF $ GetChunks id
{-# INLINABLE getChunks #-}

modifyChunks :: (Chunks -> Chunks) -> F MemoryMonad ()
modifyChunks mut = liftF $ ModifyChunks mut ()
{-# INLINABLE modifyChunks #-}

getBitfield :: F MemoryMonad BF.BitField
getBitfield = liftF $ ReadBitfield id
{-# INLINABLE getBitfield #-}

modifyBitfield :: (BF.BitField -> BF.BitField) -> F MemoryMonad ()
modifyBitfield mut = liftF $ ModifyBitfield mut ()
{-# INLINABLE modifyBitfield #-}

getAvailability :: F MemoryMonad AvailabilityData
getAvailability = liftF $ ReadAvailability id
{-# INLINABLE getAvailability #-}

modifyAvailability :: (AvailabilityData -> AvailabilityData) -> F MemoryMonad ()
modifyAvailability mut = liftF $ ModifyAvailability mut ()
{-# INLINABLE modifyAvailability #-}

evalMemoryMonad :: ClientState -> MemoryMonad (STM a) -> STM a
evalMemoryMonad state (GetChunks next) = do
  chunks <- readTVar (pieceChunks state)
  next chunks
evalMemoryMonad state (ModifyChunks f next) = do
  modifyTVar' (pieceChunks state) f
  next
evalMemoryMonad state (ReadBitfield next) = do
  res <- readTVar (bitField state)
  next res
evalMemoryMonad state (ModifyBitfield mut next) = do
  modifyTVar' (bitField state) mut
  next
evalMemoryMonad state (ReadAvailability next) = do
  res <- readTVar (availabilityData state)
  next res
evalMemoryMonad state (ModifyAvailability mut next) = do
  modifyTVar' (availabilityData state) mut
  next

runMemoryMonadSTM :: ClientState -> F MemoryMonad a -> STM a
runMemoryMonadSTM state = iterM (evalMemoryMonad state)
