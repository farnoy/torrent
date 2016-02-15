{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}

-- | Provides (PieceId -> ChunkField) mapping in production and pure flavors.
module Network.BitTorrent.DownloadProgress (
  ClassToken(..)
, ProgressStorage(..)
) where

-- import qualified Control.Concurrent.STM.Map as TT
import Control.Concurrent.STM.TVar
-- import qualified STMContainers.Map as STMap
-- import Control.Monad.Identity
import Control.Monad.STM
-- import qualified Data.Map.Strict as Map
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word
import Network.BitTorrent.ChunkField
import Network.BitTorrent.Utility

class ProgressStorage (t :: ClassToken) (m :: * -> *) | t -> m where
  data Backend t :: *
  type ReturnValue t :: *

  new    :: Word32 -> m (Backend t)
  lookup :: PieceId -> Backend t -> m (Maybe ChunkField)
  insert :: PieceId -> ChunkField -> Backend t -> m (ReturnValue t)
  delete :: PieceId -> Backend t -> m (ReturnValue t)

{-
instance ProgressStorage 'Pure Identity where
  data Backend 'Pure = PureStore (Map.Map PieceId ChunkField)
  type ReturnValue 'Pure = Backend 'Pure

  new _ = Identity $ PureStore Map.empty
  lookup k (PureStore map)   = Identity $ Map.lookup k map
  insert k v (PureStore map) = Identity $ PureStore $ Map.insert k v map
  delete k (PureStore map)   = Identity $ PureStore $ Map.delete k map
  -}

instance ProgressStorage 'Production STM where
  data Backend 'Production = ProductionStore (Vector (TVar (Maybe ChunkField)))
  type ReturnValue 'Production = ()

  new count = fmap ProductionStore $ Vector.replicateM (fromIntegral count) (newTVar Nothing)
  lookup (PieceId k) (ProductionStore vec) = readTVar $ (Vector.!) vec (fromIntegral k)
  {-# INLINABLE lookup #-}
  insert (PieceId k) v (ProductionStore vec) = writeTVar ((Vector.!) vec (fromIntegral k)) (Just v)
  {-# INLINABLE insert #-}
  delete (PieceId k) (ProductionStore vec) = writeTVar ((Vector.!) vec (fromIntegral k)) Nothing
  {-# INLINABLE delete #-}

{-
instance ProgressStorage 'Production STM where
  data Backend 'Production = ProductionStore (TT.Map PieceId ChunkField)
  type ReturnValue 'Production = ()

  new _ = fmap ProductionStore TT.empty
  lookup k (ProductionStore map)   = TT.lookup k map
  {-# INLINABLE lookup #-}
  insert k v (ProductionStore map) = TT.insert k v map
  {-# INLINABLE insert #-}
  delete k (ProductionStore map)   = pure ()

instance ProgressStorage 'Production STM where
  data Backend 'Production = ProductionStore (STMap.Map PieceId ChunkField)
  type ReturnValue 'Production = ()

  new = fmap ProductionStore STMap.new
  lookup k (ProductionStore map)   = STMap.lookup k map
  {-# INLINABLE lookup #-}
  insert k v (ProductionStore map) = STMap.insert v k map
  delete k (ProductionStore map)   = STMap.delete k map
 -}

{-
instance ProgressStorage 'Production STM where
  data Backend 'Production = ProductionStore (TVar (Map PieceId ChunkField))
  type ReturnValue 'Production = ()

  new = fmap ProductionStore $ newTVar Map.empty
  lookup k (ProductionStore map) = readTVar map >>= pure . Map.lookup k
  {-# INLINABLE lookup #-}
  insert k v (ProductionStore map) = modifyTVar' map (Map.insert k v)
  delete k (ProductionStore map)   = modifyTVar' map (Map.delete k)
  -}
