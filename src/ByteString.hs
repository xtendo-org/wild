module ByteString
    ( readFile
    , writeFile
    , range
    , module Data.ByteString.Char8
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception (bracket)
import System.IO (hClose)
import Data.ByteString.Char8 hiding (readFile, writeFile)
import qualified Data.ByteString as B
import System.Posix.ByteString

range :: Int -> Int -> ByteString -> ByteString
range start end = B.take (end - start) . B.drop start

defaultFlags :: OpenFileFlags
defaultFlags = OpenFileFlags
    { append = False
    , exclusive = False
    , noctty = True
    , nonBlock = False
    , trunc = False
    }

-- | Read an entire file at the 'RawFilePath' strictly into a 'ByteString'.
readFile :: RawFilePath -> IO ByteString
readFile path = bracket open hClose B.hGetContents
  where
    open = openFd path ReadOnly Nothing defaultFlags >>= fdToHandle

-- | Write a 'ByteString' to a file at the 'RawFilePath'.
writeFile :: RawFilePath -> ByteString -> IO ()
writeFile path content = bracket open hClose (`B.hPut` content)
  where
    open = createFile path dfm >>= fdToHandle
    dfm = unionFileModes ownerReadMode ownerWriteMode
