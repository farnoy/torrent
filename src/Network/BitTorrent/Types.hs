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
import qualified Network.BitTorrent.PeerSelection as PS
import Network.BitTorrent.Utility
import Network.Socket
import System.IO

maxRequestsPerPeer :: Word8
maxRequestsPerPeer = 8

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

data SharedMessage = RequestPiece | WakeUp deriving (Eq, Show)

defaultChunkSize :: Word32
defaultChunkSize = 2 ^ (14 :: Word32)

chunksInPieces :: Word32 -> Word32 -> Word32
chunksInPieces = divideSize
{-# INLINABLE chunksInPieces #-}

expectedPieceSize :: Word32 -> Word32 -> Word32 -> Word32
expectedPieceSize totalSize pix pSize =
  if pix >= pCount
    then if totalSize `rem` pSize == 0
         then pSize
         else totalSize `rem` pSize
    else pSize
  where pCount = divideSize totalSize pSize - 1
{-# INLINABLE expectedPieceSize #-}

expectedChunkSize :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
expectedChunkSize totalSize pix cix pSize cSize =
  if cix >= chunksInPiece
    then if expectedPSize `rem` cSize == 0
         then cSize
         else expectedPSize `rem` cSize
    else cSize
  where expectedPSize = expectedPieceSize totalSize pix pSize
        chunksInPiece = chunksInPieces expectedPSize cSize
{-# INLINABLE expectedChunkSize #-}
