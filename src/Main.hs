module Main where

import Prelude hiding ((++))
import Control.Monad
import System.IO.Error

import System.Posix.ByteString
import System.Exit hiding (die)

-- import Control.Lens
-- import Network.Wreq

import ByteString (ByteString)
import qualified ByteString as B
import qualified Pack
import RawFilePath
import Parser

(++) :: Monoid m => m -> m -> m
(++) = mappend

updateTargets :: [RawFilePath]
updateTargets =
    [ "README.md"
    ]

main :: IO ()
main = do
    homePath <- getEnv "HOME" >>= \ m -> case m of
        Nothing -> die ["No $HOME?!"]
        Just p -> return p
    cabalPath <- getDirectoryContentsSuffix "." ".cabal" >>= \ s -> case s of
        [] -> error "no cabal file found in the current directory"
        (x : _) -> return x
    cabal <- parseCabal <$> readCatch cabalPath  >>= \ m -> case m of
        Left e -> die [e]
        Right c -> return c
    let binPaths = map (homePath ++ "/.local/bin/" ++) $ cabalExecs cabal
    versions <- parseChangeLog <$> catchIOError
        (B.readFile "/CHANGELOG.md")
        (const $ error "Failed reading CHANGELOG.md")
    -- if length versions <= 1

    void $ runFail "stack" ["install"]
    forM_ binPaths $ \ bin -> do
        runFail "strip"
            [ "--strip-all"
            , "--remove-section=.comment", "--remove-section=.note"
            , bin
            ] >>= B.putStrLn
        runFail "upx" ["-9", bin] >>= B.putStrLn

    -- update files
    -- git add them
    -- git commit (with -t)
    -- send them to github
  where
    readCatch :: RawFilePath -> IO ByteString
    readCatch path = catchIOError (B.readFile path) $
        const $ B.putStrLn ("Failed reading " ++ path) *> exitFailure

runFail :: RawFilePath -> [ByteString] -> IO ByteString
runFail cmd args = do
    x <- run cmd args
    case x of
        Left c -> do
            B.putStrLn $ mconcat
                ["command fail: ", cmd, " (", Pack.int c, ")"]
            exitFailure
        Right output -> return output

die :: [ByteString] -> IO a
die e = B.putStrLn (mconcat e) *> exitFailure
