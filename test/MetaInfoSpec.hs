{-# LANGUAGE OverloadedStrings #-}
module MetaInfoSpec where

import qualified Data.Map.Strict as Map
import Network.BitTorrent.Bencoding
import Network.BitTorrent.MetaInfo
import Test.Hspec

spec :: SpecWith ()
spec = do
  let m = Map.fromList [ ("announce", String "https://test.com")
                       , ("creation date", Number 15)
                       , ("info", Dictionary (Map.fromList
                         [ ("piece length", Number 1)
                         , ("pieces", String "asdfasd")
                         , ("length", Number 568123)
                         , ("name", String "test.file")
                         ] ))
                       ]

  it "parses a valid document" $
    announce <$> parseMetaInfo (Dictionary m) `shouldBe` Just "https://test.com"

  it "parses without optional arguments" $
    let m' = Map.delete "creation date" m
    in creationDate <$> parseMetaInfo (Dictionary m') `shouldBe` Just Nothing
