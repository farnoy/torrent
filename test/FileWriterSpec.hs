{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FileWriterSpec where

import Control.Concurrent.MVar (newMVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Sequence as Seq
import Data.Word
import qualified Network.BitTorrent.FileWriter as FW
import Network.BitTorrent.Utility
import System.Directory
import System.IO

import SpecHelper
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series

data RangeFile = RangeFile Word32 FilePath deriving(Show)

instance Monad m => Serial m RangeFile where
  series = cons2 RangeFile

parseRangeFiles :: [RangeFile] -> (Word32, [(Word32, Word32, FilePath)])
parseRangeFiles = foldl folder (0, [])
  where folder (offset, list) (RangeFile size path) =
          (offset + size, list ++ [(offset, offset + size, path)])

openFiles :: [(Word32, Word32, FilePath)] -> BL.ByteString -> IO [(BL.ByteString, (Word32, Word32, Handle))]
openFiles files filler = getTemporaryDirectory >>= \dir -> traverse (opener dir) files
  where opener dir (lo, hi, _) = do
          (path, handle) <- openTempFile dir "test"
          removeFile path
          let content = BL.take (fromIntegral $ hi - lo) $ BL.drop (fromIntegral lo) filler
          BL.hPut handle content
          return (content, (lo, hi, handle))

testString :: BL.ByteString
testString = BL.cycle $ BLC.pack "kek dópa 13 pietnascie"

spec :: SpecWith ()
spec = do
  describe "FileWriter" $ do
    describe "read" $ do
      it "property test" $ property $ \rangeFiles -> monadic $ do
        let (totalSize, parsedFiles) = parseRangeFiles rangeFiles
        lock <- newMVar ()
        openedFiles <- openFiles parsedFiles testString
        return $ forAll $ \(lo, hi) -> lo < hi && hi < totalSize ==> monadic $ do
          dataRead <- FW.read (Seq.fromList $ snd <$> openedFiles) lock lo (hi - lo)
          let actual = BL.toStrict $ BL.take (fromIntegral $ hi - lo) $ BL.drop (fromIntegral lo) testString
          return $ dataRead == actual

    describe "write" $ do
      it "property test" $ property $ \rangeFiles -> monadic $ do
        let (totalSize, parsedFiles) = parseRangeFiles rangeFiles
        lock <- newMVar ()
        openedFiles <- openFiles parsedFiles testString
        return $ forAll $ \(lo, hi) -> lo < hi && hi < totalSize ==>
          forAll $ \toWrite -> (fromIntegral $ B.length toWrite) + lo == hi ==> monadic $ do
            let hdls = Seq.fromList $ snd <$> openedFiles
            FW.write hdls lock lo toWrite
            readBack <- FW.read hdls lock lo (hi - lo)
            return $ toWrite == readBack

