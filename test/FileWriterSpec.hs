{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FileWriterSpec where

import Control.Concurrent.MVar (newMVar)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (chr)
import qualified Data.Sequence as Seq
import Data.Word
import qualified Network.BitTorrent.FileWriter as FW
import System.Directory
import System.IO

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

data RangeFile = RangeFile Word64 FilePath deriving(Show)

readableChar :: Gen Word8
readableChar = choose (48, 122)

readableGen :: Gen String
readableGen = listOf1 (chr . fromIntegral <$> readableChar)

instance Arbitrary RangeFile where
  arbitrary = RangeFile <$> choose (1, 8192) <*> readableGen
  shrink (RangeFile len path) = (`RangeFile` path) <$> shrinkIntegral len

parseRangeFiles :: [RangeFile] -> (Word64, [(Word64, Word64, FilePath)])
parseRangeFiles = foldl folder (0, [])
  where folder (offset, list) (RangeFile size path) =
          (offset + size, list ++ [(offset, offset + size, path)])

openFiles :: [(Word64, Word64, FilePath)] -> IO [(BL.ByteString, (Word64, Word64, Handle))]
openFiles files = getTemporaryDirectory >>= \dir -> traverse (opener dir) files
  where opener dir (lo, hi, _) = do
          (path, handle) <- openTempFile dir "test"
          removeFile path
          let content = BL.take (fromIntegral $ hi - lo) $ BL.drop (fromIntegral lo) testString
          BL.hPut handle content
          return (content, (lo, hi, handle))

testString :: BL.ByteString
testString = BL.cycle $ BLC.pack "kek dópa 13 pietnascie"

spec :: SpecWith ()
spec = do
  describe "FileWriter" $ do
    describe "read" $ do
      prop "property test" $ \rangeFiles ->
        let (totalSize, parsedFiles) = parseRangeFiles rangeFiles
        in totalSize > 0 ==> ioProperty $ do
          lock <- newMVar ()
          openedFiles <- openFiles parsedFiles
          let generator = do
                lo <- choose (0, totalSize - 1)
                hi <- choose (lo, totalSize)
                return (lo, hi)
          return $ forAll generator $ \(lo, hi) -> ioProperty $ do
            dataRead <- FW.read (Seq.fromList $ snd <$> openedFiles) lock lo (hi - lo)
            let actual = BL.toStrict $ BL.take (fromIntegral $ hi - lo) $ BL.drop (fromIntegral lo) testString
            return $ dataRead === actual

    describe "write" $ do
      prop "property test" $ \rangeFiles ->
        let (totalSize, parsedFiles) = parseRangeFiles rangeFiles
        in totalSize > 0 ==> ioProperty $ do
          lock <- newMVar ()
          openedFiles <- openFiles parsedFiles
          let generator = do
                str <- infiniteListOf readableChar
                lo <- choose (0, totalSize - 1)
                hi <- choose (lo, totalSize)
                return (lo, hi, BL.toStrict $ BL.take (fromIntegral $ hi - lo) $ BL.pack str)

          return $ forAll generator $ \(lo, hi, toWrite) -> ioProperty $ do
            let hdls = Seq.fromList $ snd <$> openedFiles
            FW.write hdls lock lo toWrite
            readBack <- FW.read hdls lock lo (hi - lo)
            return $ toWrite === readBack
