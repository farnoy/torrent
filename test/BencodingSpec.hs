{-# LANGUAGE OverloadedStrings #-}
module BencodingSpec where

import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Map.Strict as Map
import Lens.Family2
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Bencoding.Lenses
import SpecHelper
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: SpecWith ()
spec = do
  prop "parses values" $ \s ->
    AC.maybeResult (AC.parse value (serialize s)) `shouldBe` Just s

  describe "lenses" $ do
    describe "bstring" $ do
      it "gets the value when available" $
        String "s" ^? bstring `shouldBe` Just "s"

      it "gets default when absent" $
        Number 3 ^? bstring `shouldBe` Nothing


    describe "bnumber" $ do
      it "gets the value when available" $
        Number 5 ^? bnumber `shouldBe` Just 5

      it "gets default when absent" $
        String "test" ^? bnumber `shouldBe` Nothing

    describe "blist" $ do
      it "gets the value when available" $
        List [Number 5] ^? blist `shouldBe` Just (Number 5)

      it "gets default when absent" $
        List [] ^? blist `shouldBe` Nothing

      it "supports nesting" $
        List [Number 5] ^? (blist . bnumber) `shouldBe` Just 5

    describe "bkey" $ do
      it "retrieves keys from dictionaries" $
        let dict = Dictionary (Map.insert "test" (String "true") Map.empty)
        in dict ^? (bkey "test" . bstring) `shouldBe` Just "true"

      it "retrieves nothing when not a dictionary" $
        Number 5 ^? bkey "test" `shouldBe` Nothing

    it "works with deep nesting" $
      let complex = Dictionary (Map.insert "test" (List [String "true", Number 5]) Map.empty)
      in complex ^? (bkey "test" . blist . bstring) `shouldBe` Just "true"
