{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}

-- | Provides atomic transactions on shared memory.
module Network.BitTorrent.MemoryMonad (
  MemoryMonad(..)

-- ops
, getDownloadProgress
, setDownloadProgress
, removeDownloadProgress
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
import Debug.Trace (traceMarkerIO)
import GHC.Conc (unsafeIOToSTM)
import qualified Network.BitTorrent.BitField as BF
import qualified Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.DownloadProgress as DP
import Network.BitTorrent.Types
import Network.BitTorrent.Utility

-- | Encodes memory operations.
data MemoryMonad a = GetDownloadProgress PieceId (Maybe CF.ChunkField -> a)
                   | SetDownloadProgress PieceId CF.ChunkField a
                   | RemoveDownloadProgress PieceId a
                   | ReadBitfield (BF.BitField -> a)
                   | ModifyBitfield (BF.BitField -> BF.BitField) a
                   {-
                   | ReadAvailability (AvailabilityData -> a)
                   | ModifyAvailability (AvailabilityData -> AvailabilityData) a
                   -}
                   | ReadRequestablePieces (IntSet -> a)
                   | ModifyRequestablePieces (IntSet -> IntSet) a
                   deriving(Functor)

getDownloadProgress :: PieceId -> F MemoryMonad (Maybe CF.ChunkField)
getDownloadProgress piece = liftF $ GetDownloadProgress piece id
{-# INLINABLE getDownloadProgress #-}

setDownloadProgress :: PieceId -> CF.ChunkField -> F MemoryMonad ()
setDownloadProgress piece cf = liftF $ SetDownloadProgress piece cf ()
{-# INLINABLE setDownloadProgress #-}

removeDownloadProgress :: PieceId -> F MemoryMonad ()
removeDownloadProgress piece = liftF $ RemoveDownloadProgress piece ()
{-# INLINABLE removeDownloadProgress #-}

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

evalMemoryMonad :: TorrentState 'Production -> MemoryMonad (STM a) -> STM a
evalMemoryMonad state (GetDownloadProgress piece next) = do
  cf <- DP.lookup piece (torrentStateDownloadProgress state)
  next cf
evalMemoryMonad state (SetDownloadProgress piece cf next) = do
  DP.insert piece cf (torrentStateDownloadProgress state)
  next
evalMemoryMonad state (RemoveDownloadProgress piece next) = do
  DP.delete piece (torrentStateDownloadProgress state)
  next
evalMemoryMonad state (ReadBitfield next) = do
  res <- readTVar (torrentStateBitField state)
  next res
evalMemoryMonad state (ModifyBitfield mut next) = do
  modifyTVar' (torrentStateBitField state) (force . mut)
  next
evalMemoryMonad state (ReadRequestablePieces next) = do
  res <- readTVar (torrentStateRequestablePieces state)
  next res
evalMemoryMonad state (ModifyRequestablePieces mut next) = do
  modifyTVar' (torrentStateRequestablePieces state) (force . mut)
  next

-- | Runs the whole transaction atomically under STM.
runMemoryMonadSTM :: TorrentState 'Production -> F MemoryMonad a -> STM a
runMemoryMonadSTM state m = iterM (evalMemoryMonad state) m `orElse` tracker
  where tracker = unsafeIOToSTM (putStrLn "RETRIED" *> traceMarkerIO "retry") *> retry
