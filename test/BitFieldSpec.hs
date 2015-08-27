{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BitFieldSpec where

import Data.Word
import qualified Data.ByteString as B
import Data.List
import qualified Network.BitTorrent.BitField as BF
import SpecHelper
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype BitFieldLen = BitFieldLen Word32 deriving(Show, Eq)

instance Random BitFieldLen where
  random gen = (BitFieldLen word, gen')
               where (word, gen') = random gen

instance Arbitrary BitFieldLen where
  arbitrary = BitFieldLen <$> choose (0, 100)

setMultiple :: Word32 -> [(Word32, Bool)] -> BF.BitField
setMultiple len = go starting
  where starting = BF.newBitField len
        go field [] = field
        go field ((ix, val) : xs) = let modified = BF.set field ix val in go modified xs

spec :: SpecWith ()
spec = do
  prop "reports length properly" $ \s ->
    let field = BF.newBitField s
    in BF.length field `shouldBe` s

  prop "getting bits" $ \(BitFieldLen len, BitFieldLen nth) -> nth < len ==> (
    let field = BF.newBitField len
    in BF.get field nth === False)

  it "gets the last bit properly" $
    let field = BF.newBitField 16
    in BF.get field 15 === False

  prop "sets bits properly" $ \(BitFieldLen len, BitFieldLen nth) -> nth < len ==> (
    let field = BF.set (BF.newBitField len) nth True
    in BF.get field nth === True)

  -- regression test
  prop "setting bits does not change data size" $ \(BitFieldLen len, BitFieldLen nth) ->
    nth < len ==> (
      let field = BF.set (BF.newBitField len) nth True
      in BF.lengthRaw field === (ceiling $ fromIntegral len / 8))

  prop "setting bits does not change reported size" $ \(BitFieldLen len, BitFieldLen nth) ->
    nth < len ==> (
      let field = BF.set (BF.newBitField len) nth True
      in BF.length field === len)

  prop "sets properly the values requested" $ \(values :: [(BitFieldLen, Bool)]) ->
    nub (fst <$> values) == (fst <$> values) ==> (
         let values' = (\(BitFieldLen len, val) -> (len, val)) <$> values
      in let field = setMultiple 100 values'
      in let correct = (\(ix, val) -> BF.get field ix == val) <$> values'
      in foldl' (&&) True correct)
