module Unsafe
    ( getRef
    , homePathRef
    , cabalPathRef
    , cabalRef
    ) where

import Data.IORef
import System.IO.Unsafe

import ByteString (RawFilePath)
import Parser (Cabal)

{-# NOINLINE homePathRef #-}
homePathRef :: IORef (Maybe RawFilePath)
homePathRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE cabalPathRef #-}
cabalPathRef :: IORef (Maybe RawFilePath)
cabalPathRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE cabalRef #-}
cabalRef :: IORef (Maybe Cabal)
cabalRef = unsafePerformIO $ newIORef Nothing

getRef :: IORef (Maybe a) -> IO a -> IO a
getRef ref getter = readIORef ref >>= \ mpath -> case mpath of
    Nothing -> do
        x <- getter
        writeIORef ref (Just x)
        return x
    Just x -> return x
