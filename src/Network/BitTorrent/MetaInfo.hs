{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.MetaInfo (
  MetaInfo(..)
, InfoDictionary(..)
, parseMetaInfo
) where

import Crypto.Hash.SHA1
import Data.ByteString (ByteString)
import Data.Word
import Lens.Family2
import Network.BitTorrent.Bencoding
import Network.BitTorrent.Bencoding.Lenses

data InfoDictionary = InfoDictionary {
                      pieceLength :: Word32
                    , pieces :: ByteString
                    , private :: Maybe Bool
                    , name :: ByteString
                    , length :: Word32
                    } deriving(Eq, Show)

data MetaInfo = MetaInfo {
                  info :: InfoDictionary
                , infoHash :: ByteString
                , announce :: ByteString
                , creationDate :: Maybe Word32
                } deriving(Eq, Show)

parseMetaInfo :: BValue -> Maybe MetaInfo
parseMetaInfo bv = MetaInfo
                     <$> ((bv ^? bkey "info") >>= parseInfoDictionary)
                     <*> (hash . serialize <$> (bv ^? bkey "info"))
                     <*> bv ^? (bkey "announce" . bstring)
                     <*> pure (bv ^? (bkey "creation date" . bnumber))

parseInfoDictionary :: BValue -> Maybe InfoDictionary
parseInfoDictionary bv = InfoDictionary
                           <$> bv ^? (bkey "piece length" . bnumber)
                           <*> bv ^? (bkey "pieces" . bstring)
                           <*> pure ((==1) <$> bv ^? (bkey "private" . bnumber))
                           <*> bv ^? (bkey "name" . bstring)
                           <*> bv ^? (bkey "length" . bnumber)

