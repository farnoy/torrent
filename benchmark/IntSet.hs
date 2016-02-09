{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IntSet as IntSet

import Criterion.Main

testSet :: IntSet
testSet = IntSet.fromList [0..256]

deleter :: (IntSet -> Maybe Key) -> IntSet -> IntSet
deleter lookup set = actOn (lookup set)
  where actOn (Just k) = deleter lookup (IntSet.delete k set)
        actOn Nothing = IntSet.empty

main :: IO ()
main = defaultMain [
    bgroup "difference" [
      bench "0"   $ nf (`IntSet.difference` IntSet.singleton 0) testSet
    , bench "64"  $ nf (`IntSet.difference` IntSet.singleton 64) testSet
    , bench "128" $ nf (`IntSet.difference` IntSet.singleton 128) testSet
    , bench "256" $ nf (`IntSet.difference` IntSet.singleton 256) testSet
    ]
  , bgroup "delete" [
      bench "0"   $ nf (IntSet.delete 0) testSet
    , bench "64"  $ nf (IntSet.delete 64) testSet
    , bench "128" $ nf (IntSet.delete 128) testSet
    , bench "256" $ nf (IntSet.delete 256) testSet
    ]
  , bgroup "lookupGE" [
      bench "0"   $ nf (IntSet.lookupGE 0) testSet
    , bench "64"  $ nf (IntSet.lookupGE 64) testSet
    , bench "128" $ nf (IntSet.lookupGE 128) testSet
    , bench "256" $ nf (IntSet.lookupGE 256) testSet
    ]
  , bgroup "lookupLE" [
      bench "0"   $ nf (IntSet.lookupLE 0) testSet
    , bench "64"  $ nf (IntSet.lookupLE 64) testSet
    , bench "128" $ nf (IntSet.lookupLE 128) testSet
    , bench "256" $ nf (IntSet.lookupLE 256) testSet
    ]
  , bgroup "find" [
      bench "max" $ nf IntSet.findMax testSet
    , bench "min" $ nf IntSet.findMin testSet
    ]
  , bgroup "deleteFind" [
      bench "max" $ nf IntSet.deleteFindMax testSet
    , bench "min" $ nf IntSet.deleteFindMin testSet
    , bench "findMin & delete" $ nf (IntSet.delete . IntSet.findMin) testSet
    , bench "findMax & delete" $ nf (IntSet.delete . IntSet.findMax) testSet
    , bench "maxView" $ nf IntSet.maxView testSet
    , bench "minView" $ nf IntSet.minView testSet
    , bench "null" $ nf IntSet.null testSet
    , bench "LE 256 & delete" $ nf (fmap IntSet.delete . IntSet.lookupLE 256) testSet
    , bench "GT 0   & delete" $ nf (fmap IntSet.delete . IntSet.lookupGE 0) testSet
    ]
  ]
