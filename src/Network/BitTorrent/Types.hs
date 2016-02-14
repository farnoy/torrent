{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Exports useful types for other modules.
module Network.BitTorrent.Types (
  maxRequestsPerPeer
, PeerData(..)
, newPeer
, ClientState(..)
, SharedMessage(..)
, Chunks
, defaultChunkSize
, chunksInPiece
, expectedPieceSize
, expectedChunkSize
, PieceId(..)
, ChunkId(..)
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Data.Binary
import Data.ByteString.Internal as BI
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
-- import Hexdump
import Network.BitTorrent.BitField (BitField)
import Network.BitTorrent.ChunkField as CF
import qualified Network.BitTorrent.DownloadProgress as DP
import Network.BitTorrent.MetaInfo as Meta
-- import Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.Utility
import Network.Socket
import System.IO

-- | Describes the limit of requests in flight to a single peer.
maxRequestsPerPeer :: Word8
maxRequestsPerPeer = 64

-- | Stores information about a peer.
data PeerData = PeerData {
  amChoking :: !Bool
, amInterested :: !Bool
, peerChoking :: !Bool
, peerInterested :: !Bool
, address :: SockAddr
, peerId :: !ByteString
, peerBitField :: !BitField
, requestsLive :: !Word8
, peerDataStopping :: !Bool
} deriving(Eq, Show)

-- | Stores information about the client application.
-- Holds references to shared memory peer loops use to coordinate work.
data ClientState (t :: ClassToken) = ClientState {
  myPeerId :: ByteString
, metaInfo :: MetaInfo
, bitField :: TVar BitField
, requestablePieces :: TVar IntSet
, clientStateDownloadProgress :: DP.Backend t
, outputHandles :: Seq (Word64, Word64, Handle)
, outputLock :: MVar ()
, ourPort :: Word16
-- , availabilityData :: TVar PS.AvailabilityData
, sharedMessages :: Chan SharedMessage
}

-- | Create a new 'PeerData' structure.
newPeer :: BitField -> SockAddr -> ByteString -> PeerData
newPeer bf addr peer =
  PeerData True False True False addr peer bf 0 False
{-# INLINABLE newPeer #-}

-- | Describes shared messages that can be broadcasted to peer loops.
data SharedMessage = RequestPiece | Checkup | Exit deriving (Eq, Show)

-- | Stores download progress for pieces.
--
-- For each piece that is being downloaded, holds the 'ChunkField' and
-- the full buffer with data.
type Chunks = Map PieceId ChunkField

-- | Describes granularity of a request.
--
-- /2^14/ is the size recommended by the standard.
defaultChunkSize :: Word32
defaultChunkSize = 2 ^ (14 :: Word32)

-- | Calculates the number of chunks in a piece.
chunksInPiece :: Word32 -- ^ piece size
               -> Word32 -- ^ chunk size
               -> Word32
chunksInPiece = divideSize
{-# INLINABLE chunksInPiece #-}

-- | Calculates the piece size.
expectedPieceSize :: Word64 -- ^ total size of all pieces
                  -> Word32 -- ^ piece size
                  -> PieceId
                  -> Word32
expectedPieceSize totalSize pSize (PieceId pix) =
  if pix >= pCount
    then if totalSize `rem` pSize' == 0
         then pSize
         else fromIntegral $ totalSize `rem` pSize'
    else pSize
  where pCount = fromIntegral $ divideSize totalSize pSize' - 1
        pSize' = fromIntegral pSize
{-# INLINABLE expectedPieceSize #-}

-- | Calculates the chunk size.
expectedChunkSize :: Word64  -- ^ total size of all pieces
                  -> Word32  -- ^ piece size
                  -> Word32  -- ^ default chunk size
                  -> PieceId -- ^ piece index
                  -> ChunkId -- ^ chunk index
                  -> Word32
expectedChunkSize totalSize pSize cSize piece (ChunkId cix) =
  if (cix + 1) >= chunksCount
    then if expectedPSize `rem` cSize == 0
         then cSize
         else expectedPSize `rem` cSize
    else cSize
  where expectedPSize = expectedPieceSize totalSize pSize piece
        chunksCount = chunksInPiece expectedPSize cSize
{-# INLINABLE expectedChunkSize #-}
