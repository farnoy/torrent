{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Provides measurements of speed.
module Network.BitTorrent.LinkSpeed (
  Store()
, Bytes
, Second
, empty
, record
, recent
) where

import Control.DeepSeq
import Flow
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word

type Second = Integer
type Bytes = Word64

data Store = Store {
  session :: !Bytes
, buffer :: Seq (Second, Bytes)
}

empty :: Store
empty = Store 0 Seq.empty

record :: Second -> Bytes -> Store -> Store
record t a (Store sess buf) = Store (sess + a) (trim newBuffer)
  where newBuffer = case Seq.findIndexR ((==t) . fst) buf of
          Just ix -> Seq.adjust (\(t', a') -> force (t', a' + a)) ix buf
          Nothing -> buf Seq.|> (force (t, a))
        trim buf = Seq.drop (Seq.length buf - 60) buf

recent :: Int -> Store -> Seq Bytes
recent n s = Seq.drop (Seq.length buf - n) buf |> fmap snd
  where buf = buffer s
