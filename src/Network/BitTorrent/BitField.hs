{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.BitField (
  BitField(..)
, newBitField
, Network.BitTorrent.BitField.length
, lengthRaw
, raw
, get
, set
, completed
, fromChunkFields
, union
) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable as Foldable
import qualified Data.ByteString as B
import Data.Monoid
import Data.Word
import Network.BitTorrent.ChunkField as CF
import Network.BitTorrent.Utility

data BitField = BitField ByteString Word32 deriving(Show, Eq)

newBitField :: Word32 -> BitField
newBitField len = BitField (B.replicate (fromIntegral byteLength) 0) len
  where byteLength = divideSize len 8
{-# INLINABLE newBitField #-}

fromChunkFields :: Word32 -> [(Word32, ChunkField)] -> BitField
fromChunkFields len chunksOriginal =
  BitField (snd $ B.mapAccumL f (0, chunksOriginal) fresh) len
  where byteLength = divideSize len 8
        fresh = B.replicate (fromIntegral byteLength) 0
        f (ix, chunks) w =
          let (chunks', res) = foldl' (g ix) (chunks, w) [0..7]
          in ((ix + 8, chunks'), res)
        {-# INLINABLE f #-}
        g ix (chunks, byte) n = case chunks of
          (currentIx, cf):restChunks | currentIx == ix + n ->
            if CF.isRequested cf
              then (restChunks, byte `setBit` fromIntegral (7 - n))
              else (restChunks, byte)
          _ -> (chunks, byte)
        {-# INLINABLE g #-}


union :: BitField -> BitField -> BitField
union (BitField a len) (BitField b _) = BitField (snd $ B.mapAccumL f 0 a) len
  where f ix w = (ix + 1, w .|. B.index b ix)
        {-# INLINABLE f #-}

length :: BitField -> Word32
length (BitField _ s) = s
{-# INLINABLE length #-}

lengthRaw :: BitField -> Word32
lengthRaw (BitField b _) = fromIntegral $ B.length b
{-# INLINABLE lengthRaw #-}

raw :: BitField -> ByteString
raw (BitField b _) = b
{-# INLINABLE raw #-}

get :: BitField -> Word32 -> Bool
get (BitField field _) ix = testBit word (7 - fromIntegral ix `rem` 8)
  where byteIx =  fromIntegral $ ix `quot` 8
        word =  B.index field byteIx
{-# INLINABLE get #-}

set :: BitField -> Word32 -> Bool -> BitField
set (BitField field len) ix val = (BitField $! updatedField) $! len
  where byteIx = fromIntegral ix `quot` 8
        word = B.index field byteIx
        modifier True = setBit
        modifier False = clearBit
        updatedByte = modifier val word (7 - fromIntegral ix `rem` 8)
        updatedField = B.take byteIx field <> B.singleton updatedByte <> B.drop (byteIx + 1) field
{-# INLINABLE set #-}

completed :: BitField -> Float
completed (BitField b len) = fromIntegral (Foldable.sum (popCount <$> B.unpack b)) / fromIntegral len
{-# INLINABLE completed #-}
