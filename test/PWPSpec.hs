{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PWPSpec where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import qualified Network.BitTorrent.BitField as BF
import Network.BitTorrent.PWP

import SpecHelper
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck

spec :: SpecWith ()
spec = do
  describe "PWP" $ do
    it "decode . encode === id" $ property $ \(m :: PWP) ->
      (decodeOrFail . encode) m == Right (BL.empty, BL.length $ encode m, m)

  describe "BHandshake" $ do
    it "decode . encode === id" $ property $ \a@(BHandshake hash id) ->
      B.length hash == 20 && B.length id == 20 ==>
        (decodeOrFail . encode) a == Right (BL.empty, BL.length $ encode a, a)
