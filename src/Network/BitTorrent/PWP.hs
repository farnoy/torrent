{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Network.BitTorrent.PWP where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

protocolString :: ByteString
protocolString = "BitTorrent protocol"

data PWP = KeepAlive
         | Choke
         | Unchoke
         | Interested
         | Uninterested
         | Have Word32
         | Bitfield ByteString
         | Request Word32 Word32 Word32
         | Piece Word32 Word32 ByteString
         | Cancel Word32 Word32 Word32
         deriving(Show, Eq)

putPieceDescription :: Word32 -> Word32 -> Word32 -> Put
putPieceDescription p o l = traverse put [p, o, l] >> return ()
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
  put (Piece piece offset d) = do
    put (fromIntegral $ 9 + B.length d :: Word32)
    put (7 :: Word8)
    put piece
    put offset
    putByteString d
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
          7 -> Piece <$> get <*> get
                     <*> getByteString (fromIntegral len - 9)
          8 -> Cancel <$> get <*> get <*> get
          _ -> fail "incorrect!"
  {-# INLINABLE get #-}

data BHandshake = BHandshake ByteString ByteString deriving(Show, Eq)

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
