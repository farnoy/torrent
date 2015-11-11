{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ChunkFieldSpec where

import Data.Word
import qualified Data.ByteString as B
import Data.List
import qualified Network.BitTorrent.ChunkField as CF
import Network.BitTorrent.Utility
import SpecHelper
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck

spec :: SpecWith ()
spec = do
  it "returns incomplete chunks in order" $ property $ \n ->
    n > 1 ==>
      let field = CF.newChunkField n
          Just (field', next) = CF.getNextChunk field
      in next == (ChunkId 0) &&
         (snd <$> CF.getNextChunk field') == Just (ChunkId 1)

  it "marks complete chunks properly" $ do
    let field = CF.markRequested (CF.newChunkField 2) (ChunkId 0)
    snd <$> CF.getNextChunk field `shouldBe` Just (ChunkId 1)

