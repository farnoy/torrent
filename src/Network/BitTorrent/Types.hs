-- | Exports useful types for other modules.
module Network.BitTorrent.Types where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Data.Binary
import Data.ByteString.Internal as BI
import Data.Map.Strict (Map)
-- import Hexdump
import Network.BitTorrent.BitField (BitField)
import Network.BitTorrent.ChunkField as CF
import Network.BitTorrent.MetaInfo as Meta
import qualified Network.BitTorrent.PieceSelection as PS
import Network.BitTorrent.Utility
import Network.Socket
import System.IO

-- | Describes the limit of requests in flight to a single peer.
maxRequestsPerPeer :: Word8
maxRequestsPerPeer = 8

-- | Stores information about a peer.
data PeerData = PeerData {
  amChoking :: Bool
, amInterested :: Bool
, peerChoking :: Bool
, peerInterested :: Bool
, address :: SockAddr
, peerId :: ByteString
, peerBitField :: BitField
, requestsLive :: Word8
} deriving(Eq, Show)

-- | Create a new 'PeerData' structure.
newPeer :: BitField -> SockAddr -> ByteString -> PeerData
newPeer bf addr peer =
  PeerData True False True False addr peer bf 0
{-# INLINABLE newPeer #-}

-- | Stores information about the client application.
-- Holds references to shared memory peer loops use to coordinate work.
data ClientState = ClientState {
  myPeerId :: ByteString
, metaInfo :: MetaInfo
, bitField :: TVar BitField
, pieceChunks :: TVar (Map Word32 (ChunkField, ByteString))
, outputHandle :: Handle
, outputLock :: MVar ()
, ourPort :: Word16
, availabilityData :: TVar PS.AvailabilityData
, sharedMessages :: Chan SharedMessage
}

-- | Describes shared messages that can be broadcasted to peer loops.
data SharedMessage = RequestPiece | Checkup deriving (Eq, Show)

-- | Stores download progress for pieces.
--
-- For each piece that is being downloaded, holds the 'ChunkField' and
-- the full buffer with data.
type Chunks = Map Word32 (ChunkField, ByteString)

-- | Describes granularity of a request.
--
-- /2^14/ is the size recommended by the standard.
defaultChunkSize :: Word32
defaultChunkSize = 2 ^ (14 :: Word32)

-- | Calculates the number of chunks in a piece.
chunksInPieces :: Word32 -- ^ piece size
               -> Word32 -- ^ chunk size
               -> Word32
chunksInPieces = divideSize
{-# INLINABLE chunksInPieces #-}

-- | Calculates the piece size.
expectedPieceSize :: Word32 -- ^ total size of all pieces
                  -> Word32 -- ^ piece index
                  -> Word32 -- ^ piece size
                  -> Word32
expectedPieceSize totalSize pix pSize =
  if pix >= pCount
    then if totalSize `rem` pSize == 0
         then pSize
         else totalSize `rem` pSize
    else pSize
  where pCount = divideSize totalSize pSize - 1
{-# INLINABLE expectedPieceSize #-}

-- | Calculates the chunk size.
expectedChunkSize :: Word32 -- ^ total size of all pieces
                  -> Word32 -- ^ piece index
                  -> Word32 -- ^ chunk index
                  -> Word32 -- ^ piece size
                  -> Word32 -- ^ default chunk size
                  -> Word32
expectedChunkSize totalSize pix cix pSize cSize =
  if cix >= chunksInPiece
    then if expectedPSize `rem` cSize == 0
         then cSize
         else expectedPSize `rem` cSize
    else cSize
  where expectedPSize = expectedPieceSize totalSize pix pSize
        chunksInPiece = chunksInPieces expectedPSize cSize
{-# INLINABLE expectedChunkSize #-}
