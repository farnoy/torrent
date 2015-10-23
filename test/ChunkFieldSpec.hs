{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ChunkFieldSpec where

import Data.Word
import qualified Data.ByteString as B
import Data.List
import qualified Network.BitTorrent.ChunkField as CF
import SpecHelper
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: SpecWith ()
spec = do
  prop "returns incomplete chunks in order" $ \n ->
    n > 1 ==> do
      let field = CF.newChunkField n
          Just (field', next) = CF.getNextChunk field
      next `shouldBe` 0
      snd <$> CF.getNextChunk field' `shouldBe` Just 1

  it "marks complete chunks properly" $ do
    let field = CF.markRequested (CF.newChunkField 2) 0
    snd <$> CF.getNextChunk field `shouldBe` Just 1

