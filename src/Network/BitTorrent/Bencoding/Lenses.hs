{-# LANGUAGE RankNTypes #-}
-- | Provides lenses for Bencoded values.
--
-- == Example
-- Parsing a MetaInfo string:
--
-- @
-- *Bencoding> let Right result = P.parseOnly value input
-- *Bencoding> result ^? bkey "announce" . bstring
-- Just "http://tracker.archlinux.org:6969/announce"
-- *Bencoding> result ^? bkey "info" . bkey "length" . bnumber
-- Just 688914432
-- @

module Network.BitTorrent.Bencoding.Lenses where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map
import Lens.Family2
import Network.BitTorrent.Bencoding
import Prelude hiding (take)

-- | Traverses a string value.
bstring :: Traversal' BValue ByteString
bstring f (String s) = String <$> f s
bstring _ bv = pure bv

-- | Traverses a number value.
bnumber :: Traversal' BValue Integer
bnumber f (Number n) = Number <$> f n
bnumber _ bv = pure bv

-- | Traverses each element in a list.
blist :: Traversal' BValue BValue
blist f (List xs) = List <$> traverse f xs
blist _ bv = pure bv

-- | Traverses value of dictionaries under the given key.
bkey :: ByteString -> Traversal' BValue BValue
bkey k f bv@(Dictionary m) = case Map.lookup k m of
                            Just v -> f v
                            Nothing -> pure bv
bkey _ _ bv = pure bv
