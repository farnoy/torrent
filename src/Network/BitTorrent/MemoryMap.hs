{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.BitTorrent.MemoryMap (
  MemoryMap
, getPtr
, openMMap
, closeMMap
) where

import Data.Bits
import Foreign
import Foreign.C
import Protolude
import System.Posix
import System.IO.Error

data MemoryMap = MemoryMap CSize (Ptr Word8)

foreign import ccall unsafe "mmap"
  c_mmap :: Ptr Word8 -> CSize -> CInt -> CInt -> CInt -> CSize -> IO (Ptr Word8)

mmap :: CSize -> CInt -> CInt -> CInt -> CSize -> IO (Either Text MemoryMap)
mmap len prot flags fd offset = do
  region <- c_mmap nullPtr len prot flags fd offset
  case region of
    ptr | ptr == (nullPtr `plusPtr` (-1)) -> getErrno >>= \(Errno e) -> return $ Left $ show e
    ptr -> return $ Right (MemoryMap len ptr)

-- | Opens a file for ReadWrite, preallocates desired size and returns a mapping.
openMMap :: FilePath -- ^ file to open
         -> CSize    -- ^ desired size
         -> IO (Either Text MemoryMap)
openMMap path size = openFile >>= \fd -> finally (openMap fd) (closeFd fd)
  where openFile = openFd path ReadWrite (Just 0o644) defaultFileFlags
        --    PROT_READ | PROT_WRITE
        protection = 1 .|. 2
        -- TODO hugepages
        -- MAP_HUGETLB |   MAP_HUGE_2MB   | MAP_SHARED
        --    (262144 .|. (shiftL 21 26) .|. 1)
        flags = 1  -- MAP_SHARED
        openMap (Fd fd) = do
          ret <- c_fallocate fd 0 0 size
          case ret of
            0 -> do
              mapOpt <- mmap size protection flags fd 0
              return mapOpt
            _ -> getErrno >>= \(Errno e) -> return $ Left $ show e

foreign import ccall unsafe "munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "msync"
  c_msync :: Ptr Word8 -> CSize -> CSize -> IO CInt

closeMMap :: MemoryMap -> IO ()
closeMMap (MemoryMap len ptr) = do
  ret <- c_msync ptr len (2 .|. 4) -- MS_INVALIDATE | MS_SYNC
  when (ret == (-1)) $
    getErrno >>= \(Errno e) -> ioError $ userError $ "closeMMap failed to msync with errno " ++ show e
  ret <- c_munmap ptr len
  when (ret == (-1)) $
    getErrno >>= \(Errno e) -> ioError $ userError $ "closeMMap failed to munmap with errno " ++ show e

foreign import ccall unsafe "fallocate"
  c_fallocate :: CInt -> CInt -> CSize -> CSize -> IO CInt

getPtr :: MemoryMap -> Ptr Word8
getPtr (MemoryMap _ ptr) = ptr