{-# LANGUAGE OverloadedStrings #-}
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
import Network.Socket
import Test.QuickCheck
import Test.QuickCheck.Instances()

instance Arbitrary BValue where
  arbitrary = do
    selected <- elements [0, 1, 2, 3] :: Gen Int
    case selected of
      0 -> String <$> arbitrary
      1 -> Number <$> arbitrary
      2 -> do
        n <- arbitrarySizedNatural
        contents <- resize (n `quot` 2) $ replicateM n arbitrary
        return $ List contents
      _ -> do
        n <- arbitrarySizedNatural
        let pairsPure = [(,) <$> arbitrary <*> arbitrary | _ <- [0..n]]
        pairs <- resize (n `quot` 2) $ sequence pairsPure
        let m = Data.Foldable.foldl' (\m' (k, v) -> Map.insert k v m') Map.empty pairs
        return $ Dictionary m

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


