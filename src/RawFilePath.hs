-- The 'unix' module provides 'RawFilePath'-variants of all functions, but
-- higher-level wrappers of it such as 'directory' or 'process' doesn't.
-- This module provides it.
--
module RawFilePath
    ( run
    , getDirectoryContents
    , getDirectoryContentsSuffix
    ) where

import Control.Monad
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Exit
import System.Posix.ByteString

run :: RawFilePath -> [ByteString] -> IO (Either Int ByteString)
run cmd args = do
    (fd0, fd1) <- createPipe
    pid <- forkProcess $ do
        closeFd fd0
        closeFd stdOutput
        void $ dupTo fd1 stdOutput
        executeFile cmd True args Nothing
    closeFd fd1
    content <- fdToHandle fd0 >>= B.hGetContents
    getProcessStatus True False pid >>= \ mstatus -> case mstatus of
        Just status -> case status of
            Exited exitCode -> case exitCode of
                ExitSuccess -> return $ Right content
                ExitFailure c -> return $ Left c
            _ -> error $ show cmd
        Nothing -> error $ show cmd

getDirectoryContents :: RawFilePath -> IO [RawFilePath]
getDirectoryContents dirPath = bracket open close repeatRead
  where
    open = openDirStream dirPath
    close = closeDirStream
    repeatRead stream = do
        d <- readDirStream stream
        if B.length d == 0 then return [] else do
            rest <- repeatRead stream
            return $ d : rest

getDirectoryContentsSuffix :: RawFilePath -> ByteString -> IO [RawFilePath]
getDirectoryContentsSuffix dirPath suffix = bracket open close repeatRead
  where
    open = openDirStream dirPath
    close = closeDirStream
    repeatRead stream = do
        d <- readDirStream stream
        if B.length d == 0 then return [] else do
            rest <- repeatRead stream
            return $ (if suffix `B.isSuffixOf` d then (d :) else id) rest
