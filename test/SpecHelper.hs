{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SpecHelper where

import Control.Monad
import Crypto.Hash.SHA1
import Data.Binary
import Data.Binary.Get
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BC
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Network.BitTorrent.Bencoding
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.MetaInfo
import Network.BitTorrent.PWP
import Network.Socket (SockAddr(..))
import Test.SmallCheck
import Test.SmallCheck.Series

instance Monad m => Serial m B.ByteString where
  series = cons1 B.pack

instance (Monad m, Ord k, Serial m k, Serial m v) => Serial m (Map.Map k v) where
  series = cons1 Map.fromList

instance Monad m => Serial m BValue where
  series = cons1 String \/ cons1 Number \/ cons1 List \/ cons1 Dictionary

instance Monad m => Serial m Word32 where
  series = fromIntegral <$> (series :: Series m (Test.SmallCheck.Series.Positive Int))

instance Monad m => Serial m Word8 where
  series = fromIntegral <$> (series :: Series m (Test.SmallCheck.Series.Positive Int))

instance Monad m => Serial m PWP where
  series = cons0 KeepAlive \/ cons0 Choke \/ cons0 Unchoke \/ cons0 Interested
        \/ cons0 Uninterested \/ cons1 Have \/ cons1 Bitfield \/ cons3 Request
        \/ cons3 Piece \/ cons3 Cancel

instance Monad m => Serial m BHandshake where
  series = cons2 BHandshake

testAddr bytes port = SockAddrInet port (decode $ BL.pack bytes :: Word32)

testData = BL.toStrict $ BL.take (pieceSize * 48 - 80) $ BL.cycle "kopa to dopa"
pieceSize :: Integral a => a
pieceSize = 2^14 * 3 -- 3 chunks
pieceCount :: Integral a => a
pieceCount = ceiling $ fromIntegral (B.length testData) / (fromIntegral pieceSize)
hashes = go testData
  where go "" = ""
        go input = hash (B.take pieceSize input) <> go (B.drop pieceSize input)

fullBitField = BF.BitField (B.replicate (ceiling $ fromIntegral pieceCount / 8) 0xFF) pieceCount

infoDictionaryRaw =
  Dictionary $ Map.fromList [ ("piece length", Number $ fromIntegral pieceSize)
                            , ("pieces", String hashes)
                            , ("length", Number $ fromIntegral $ B.length testData)
                            , ("name", String $ BC.pack $ "output-test")
                            ]

metaInfoRaw =
  Dictionary $ Map.fromList [ ("info", infoDictionaryRaw)
                            , ("announce", String "http://tracker.archlinux.org:6969/announce")
                            ]

testMeta = fromJust $ parseMetaInfo metaInfoRaw


