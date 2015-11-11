{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BitFieldSpec where

import Data.Word
import qualified Data.ByteString as B
import Data.List
import qualified Network.BitTorrent.BitField as BF
import SpecHelper
import System.Random
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series

setMultiple :: Word32 -> [(Word32, Bool)] -> BF.BitField
setMultiple len = go starting
  where starting = BF.newBitField len
        go field [] = field
        go field ((ix, val) : xs) = let modified = BF.set field ix val in go modified xs

spec :: SpecWith ()
spec = do
  it "reports length properly" $ property $ \s ->
    let field = BF.newBitField s
    in BF.length field == s

  it "sets all bits to False on creation" $ property $ \n ->
    let field = BF.newBitField n
    in forAll $ \ix -> ix <= n ==>
      BF.get field ix == False

  it "sets bits properly" $ property $ \len ->
    let field = BF.newBitField len
    in forAll $ \ix -> ix < len ==>
      let modified = BF.set field ix True
      in BF.get modified ix == True

  it "sets only one bit at a time" $ property $ \len ->
    let field = BF.newBitField len
    in forAll $ \ix -> ix < len ==>
      let modified = BF.set field ix True
      in existsUnique $ \jx -> jx < len && BF.get modified jx == True

  -- regression test
  -- bad usage won't blow it up unexpectedly
  it "setting bits does not change data size" $ property $ \len ->
    forAll $ \ix -> ix >= len ==>
      let field = BF.set (BF.newBitField len) ix True
      in B.length (BF.raw field) == (ceiling $ fromIntegral len / 8)

  it "setting bits does not change reported size" $ property $ \len ->
    forAll $ \ix -> ix < len ==>
      let field = BF.set (BF.newBitField len) ix True
      in BF.length field == len

  it "sets properly the values requested" $ property $ \values ->
    -- nub because we don't support checking overwrites
    nub (fst <$> values) == (fst <$> values) ==>
      let values' = (\(len, val) -> (len, val)) <$> values
          field = setMultiple 100 values'
          correct = (\(ix, val) -> BF.get field ix == val) <$> values'
      in and correct

  it "calculates bitfield intersection" $ property $ changeDepth (const 6) $ \bf1 bf2 ->
    BF.length bf1 > 0 && BF.length bf1 == BF.length bf2 ==>
      let intersected = BF.intersection bf1 bf2
      in forAll $ \n -> n >= 0 && n < BF.length bf1 ==>
        case BF.get intersected n of
          True -> BF.get bf1 n && BF.get bf2 n
          False -> (BF.get bf1 n == False) || (BF.get bf2 n == False)

  it "calculates bitfield union" $ property $ changeDepth (const 6) $ \bf1 bf2 ->
    BF.length bf1 > 0 && BF.length bf1 == BF.length bf2 ==>
      let unioned = BF.union bf1 bf2
      in forAll $ \n -> n >= 0 && n < BF.length bf1 ==>
        case BF.get unioned n of
          True -> BF.get bf1 n || BF.get bf2 n
          False -> (BF.get bf1 n == False) && (BF.get bf2 n == False)

  it "calculates bitfield difference" $ property $ changeDepth (const 6) $ \bf1 bf2 ->
    BF.length bf1 > 0 && BF.length bf1 == BF.length bf2 ==>
      let diffed = BF.difference bf1 bf2
      in forAll $ \n -> n >= 0 && n < BF.length bf1 ==>
        case BF.get diffed n of
          True -> BF.get bf1 n && (BF.get bf2 n == False)
          False -> (BF.get bf1 n == False) || BF.get bf2 n
