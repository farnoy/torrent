{-# LANGUAGE RankNTypes #-}
module Network.BitTorrent.Bencoding.Lenses where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map
import Data.Word
import Lens.Family2
import Network.BitTorrent.Bencoding
import Prelude hiding (take)

bstring :: Traversal' BValue ByteString
bstring f (String s) = String <$> f s
bstring _ bv = pure bv

bnumber :: Traversal' BValue Word32
bnumber f (Number n) = Number <$> f n
bnumber _ bv = pure bv

blist :: Traversal' BValue BValue
blist f (List xs) = List <$> traverse f xs
blist _ bv = pure bv

bkey :: ByteString -> Traversal' BValue BValue
bkey k f bv@(Dictionary m) = case Map.lookup k m of
                            Just v -> f v
                            Nothing -> pure bv
bkey _ _ bv = pure bv
