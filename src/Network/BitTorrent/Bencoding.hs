{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Bencoding (
  value
, BValue(..)
, serialize
) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Word
import Prelude hiding (take)

data BValue = String ByteString
            | Number Word32
            | List [BValue]
            | Dictionary (Map ByteString BValue)
              deriving(Eq,Show)

string :: Parser BValue
string = do
  n <- P.decimal
  _ <- P.char ':'
  String <$> P.take n
{-# INLINABLE string #-}

number :: Parser BValue
number = Number <$> (P.char 'i' *> P.signed P.decimal <* P.char 'e')
{-# INLINABLE number #-}

list :: Parser BValue
list = List <$> (P.char 'l' *> P.many' value <* P.char 'e')
{-# INLINABLE list #-}

dictionary :: Parser BValue
dictionary = do
  _ <- P.char 'd'
  pairs <- P.many' ((,) <$> string <*> value)
  _ <- P.char 'e'
  let pairs' = (\(String s, v) -> (s, v)) <$> pairs
  let map' = Map.fromList pairs'
  return $ Dictionary map'
{-# INLINABLE dictionary #-}

value :: Parser BValue
value = string <|> number <|> list <|> dictionary
{-# INLINABLE value #-}

serialize :: BValue -> ByteString
serialize (String s) = BC.pack (show $ B.length s) <> ":" <> s
serialize (Number s) = "i" <> BC.pack (show s) <> "e"
serialize (List s) = "l" <> (BC.intercalate "" . toList . fmap serialize $ s) <> "e"
serialize (Dictionary m) = "d" <> (BC.intercalate "" . Map.foldrWithKey folder [] $ m) <> "e"
                           where folder :: ByteString -> BValue -> [ByteString] -> [ByteString]
                                 folder k v a = (serialize (String k) <> serialize v) : a
{-# INLINABLE serialize #-}
