-- | Provides (PieceId -> ChunkField) mapping in production and pure flavors.
module Network.BitTorrent.DownloadProgress (
  ProgressStorage()
, new
, Network.BitTorrent.DownloadProgress.lookup
, insert
, delete
) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word
import Network.BitTorrent.ChunkField
import Network.BitTorrent.Utility

data ProgressStorage = ProgressStorage (Vector (TVar (Maybe ChunkField)))

new :: Word32 -> STM ProgressStorage
new count = ProgressStorage <$> Vector.replicateM (fromIntegral count) (newTVar Nothing)

lookup :: PieceId -> ProgressStorage -> STM (Maybe ChunkField)
lookup (PieceId k) (ProgressStorage vec) = readTVar $ (Vector.!) vec (fromIntegral k)

insert :: PieceId -> ChunkField -> ProgressStorage -> STM ()
insert (PieceId k) v (ProgressStorage vec) = writeTVar ((Vector.!) vec (fromIntegral k)) (Just v)

delete :: PieceId -> ProgressStorage -> STM ()
delete (PieceId k) (ProgressStorage vec) = writeTVar ((Vector.!) vec (fromIntegral k)) Nothing
