{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Provides means to communicate over the Peer Wire Protocol.
-- You can encode and decode messages using the 'Binary' instances.
module Network.BitTorrent.PWP (
  PWP(..)
, BHandshake(..)
, MessageType(..)
, bytesNeededForParse
) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable

protocolString :: ByteString
protocolString = "BitTorrent protocol"

-- | Encodes all the messages in PWP.
data PWP = KeepAlive
         | Choke
         | Unchoke
         | Interested
         | Uninterested
         | Have Word32 -- ^ PieceId
         | Bitfield ByteString
         | Request Word32 Word32 Word32 -- ^ PieceId Offset Length
         | Piece Word32 Word32 Word32 -- ^ PieceId Offset Data
         | Cancel Word32 Word32 Word32 -- ^ PieceId Offset Length
         deriving(Show, Eq)

putPieceDescription :: Word32 -> Word32 -> Word32 -> Put
putPieceDescription p o l = traverse_ put [p, o, l]
{-# INLINABLE putPieceDescription #-}

instance Binary PWP where
  put KeepAlive =
    put (0 :: Word32)
  put Choke = do
    put (1 :: Word32)
    put (0 :: Word8)
  put Unchoke = do
    put (1 :: Word32)
    put (1 :: Word8)
  put Interested = do
    put (1 :: Word32)
    put (2 :: Word8)
  put Uninterested = do
    put (1 :: Word32)
    put (3 :: Word8)
  put (Have pieceId) = do
    put (5 :: Word32)
    put (4 :: Word8)
    put (pieceId :: Word32)
  put (Bitfield field) = do
    putWord32be $ fromIntegral $ 1 + B.length field
    put (5 :: Word8)
    putByteString field
  put (Request piece offset len) = do
    put (13 :: Word32)
    put (6 :: Word8)
    putPieceDescription piece offset len
  put (Piece piece offset len) = do
    put (fromIntegral $ 9 + len :: Word32)
    put (7 :: Word8)
    put piece
    put offset
  put (Cancel piece offset len) = do
    put (13 :: Word32)
    put (8 :: Word8)
    putPieceDescription piece offset len
  {-# INLINABLE put #-}

  get = do
    len <- get :: Get Word32
    case len of
      0 -> return KeepAlive
      _ -> do
        messageId <- getWord8
        case messageId of
          0 -> return Choke
          1 -> return Unchoke
          2 -> return Interested
          3 -> return Uninterested
          4 -> Have <$> get
          5 -> do
            let payloadLength = len - 1
            bytestring <- getByteString (fromIntegral payloadLength)
            return $ Bitfield bytestring
          6 -> Request <$> get <*> get <*> get
          7 -> Piece <$> get <*> get <*> return (len - 9)
                     -- <*> getByteString (fromIntegral len - 9)
          8 -> Cancel <$> get <*> get <*> get
          _ -> fail "incorrect!"
  {-# INLINABLE get #-}

data MessageType = MsgKeepAlive
                 | MsgChoke
                 | MsgUnchoke
                 | MsgInterested
                 | MsgUninterested
                 | MsgHave
                 | MsgBitfield
                 | MsgRequest
                 | MsgPiece
                 | MsgCancel
                 deriving(Show, Eq)

instance Binary MessageType where
  put = undefined
  get = do
    len <- get :: Get Word32
    case len of
      0 -> return MsgKeepAlive
      _ -> do
        messageId <- getWord8
        case messageId of
          0 -> return MsgChoke
          1 -> return MsgUnchoke
          2 -> return MsgInterested
          3 -> return MsgUninterested
          4 -> return MsgHave
          5 -> return MsgBitfield
          6 -> return MsgRequest
          7 -> return MsgPiece
          8 -> return MsgCancel
          _ -> fail "incorrect!"
  {-# INLINABLE get #-}

bytesNeededForParse :: Word32 -> MessageType -> Word32
bytesNeededForParse _ MsgKeepAlive = 0
bytesNeededForParse _ MsgChoke = 0
bytesNeededForParse _ MsgUnchoke = 0
bytesNeededForParse _ MsgInterested = 0
bytesNeededForParse _ MsgUninterested = 0
bytesNeededForParse _ MsgHave = 4
bytesNeededForParse len MsgBitfield = len - 1
bytesNeededForParse _ MsgRequest = 3 * 4
bytesNeededForParse len MsgPiece = 2 * 4
bytesNeededForParse _ MsgCancel = 3 * 4

-- | Encodes the BHandshake message.
data BHandshake = BHandshake
  { handshakeInfoHash :: ByteString -- ^ Info hash of the torrent
  , handshakePeerId   :: ByteString -- ^ Peer's ID
  } deriving(Show, Eq)

instance Binary BHandshake where
  put (BHandshake infoHash peerId) = do
    putWord8 (fromIntegral $ B.length protocolString)
    putByteString protocolString
    replicateM_ 8 (putWord8 0)
    putByteString infoHash
    putByteString peerId

  get = do
    protoSize <- get :: Get Word8
    replicateM_ (fromIntegral protoSize) getWord8
    skip 8
    infoHash <- getByteString 20
    peerId   <- getByteString 20
    return $ BHandshake infoHash peerId
