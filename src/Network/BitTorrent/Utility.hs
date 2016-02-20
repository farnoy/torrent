{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.BitTorrent.Utility (
  ClassToken(..)
, divideSize
, boolToWord
, fileOverlap
, PieceId(..)
, ChunkId(..)
, mySplice
, spliceAll
, recvAll
) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Base
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Sequence
import Data.Word
import Data.Hashable
import Foreign
import Foreign.C
import Foreign.C.Error
import GHC.Generics (Generic)
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Posix

data ClassToken = Production | Pure

divideSize :: Integral a => a -> a -> a
divideSize a b | a `rem` b > 0 = (a `quot` b) + 1
divideSize a b = a `quot` b
{-# INLINABLE divideSize #-}

boolToWord :: Integral a => Bool -> a
boolToWord True = 1
boolToWord False = 0
{-# INLINABLE boolToWord #-}

-- | Holds the piece id.
newtype PieceId = PieceId Word32 deriving(Eq, Show, Ord, Generic, NFData, Hashable)

-- | Holds the chunk id.
newtype ChunkId = ChunkId Word32 deriving(Eq, Show, Ord, Generic, NFData)

-- | Finds overlaps of any range with given array.
fileOverlap :: Seq (Word64, Word64, Word64, a) -- ^ Left-inclusive ranges to look in + user data
            -> Word64                  -- ^ lower bound to lookup
            -> Word64                  -- ^ upper bound to lookup
            -> Seq (Word64, Word64, Word64, a)
fileOverlap ranges lo hi = rightAdjusted
  where leftDropped = dropWhileL (\(_, _, h, _) -> h <= lo) ranges
        leftAdjusted = case viewl leftDropped of
          (base, leftLo, leftHi, leftC) :< leftDropped -> (base, max leftLo lo, leftHi, leftC) <| leftDropped
          _ -> leftDropped

        rightDropped = takeWhileL (\(_, l, _, _) -> hi >= l) leftAdjusted
        rightAdjusted = case viewr rightDropped of
          rightDropped :> (base, rightLo, rightHi, rightC) -> rightDropped |> (base, rightLo, min rightHi hi, rightC)
          EmptyR -> rightDropped

spliceAll :: Fd -> Ptr Int64 -> Bool -> Fd -> Ptr Int64 -> Bool -> Word32 -> Word -> IO Bool
spliceAll inFd inOff waitIn outFd outOff waitOut len flags = go len
  where go remaining | remaining == 0 = return True
        go remaining = do
          when waitIn $ threadWaitRead inFd
          when waitOut $ threadWaitWrite outFd
          written <- mySplice inFd inOff outFd outOff remaining flags
          case written of
            (-1) -> do
            {-
              Errno e <- getErrno
              putStr "errno = "
              print e
              putStr "eBadF = "
              print (let Errno i = eBADF in i)
              -}
              return False
            _ -> go (remaining - fromIntegral written)
{-# INLINABLE spliceAll #-}

foreign import ccall unsafe "splice"
  mySplice :: Fd -> Ptr (Int64) -> Fd -> Ptr (Int64) -> Word32 -> Word -> IO Int32

recvAll :: Socket -> Int -> IO ByteString
recvAll sock len = go len ""
  where go 0 accu = return accu
        go remaining accu = do
          read <- NSB.recv sock remaining
          if B.length read > 0
            then go (remaining - B.length read) read
            else return accu
{-# INLINABLE recvAll #-}
