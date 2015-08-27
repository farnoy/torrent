{-# LANGUAGE OverloadedStrings #-}
module SpecHelper where

import Control.Monad
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Network.BitTorrent.Bencoding
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


