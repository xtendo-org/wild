module Main where

import Prelude hiding ((++))
import Control.Monad
import Control.Exception
import System.IO
import System.IO.Error
import qualified Data.Text.Encoding as T

import System.Posix.ByteString
import System.Exit hiding (die)

import System.Console.CmdArgs.Explicit

import ByteString (ByteString)
import qualified ByteString as B
import qualified Pack
import RawFilePath
import Parser
import Format
import GitHub

(++) :: Monoid m => m -> m -> m
(++) = mappend

data Cmd
    = CmdHelp
    | CmdAll
    | CmdPrep
    | CmdVersion
    | CmdUpload

arguments :: Mode Cmd
arguments = modes "wild" CmdHelp
    "automate releasing Haskell projects"
    [ m "all" CmdAll "run all wild steps"
    , m "prep" CmdPrep "build project to prepare for release"
    , m "version" CmdVersion "change the version string\
        \ in files and commit those changes"
    , m "upload" CmdUpload "create a new release draft in GitHub\
        \ and upload executables as its assets"
    ]
  where
    m :: Name -> Cmd -> Help -> Mode Cmd
    m name cmd help = mode name cmd help
        (flagArg (\_ c -> Right c) "") []

main :: IO ()
main = do
    Conf{..} <- fmap (either error id . dec) $
        catchIOError (B.readFile "wild.json") $ const $ do
            B.writeFile "wild.json" $ enc initConf
            B.putStrLn "Created wild.json with default configuration."
            exitFailure

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
    versions <- parseChangeLog <$> readCatch "CHANGELOG.md"
    token <- readCatch $ let
        path = T.encodeUtf8 tokenPath
        in if B.head path == '~' then homePath ++ B.tail path else path

    run "stack" ["install"]
    forM_ binPaths $ \ bin -> do
        run "strip"
            [ "--strip-all"
            , "--remove-section=.comment", "--remove-section=.note"
            , bin
            ]
        run "upx" ["-9", bin]

    case versions of
        [] -> die ["version not found in change log"]
        [_] -> return ()
        (newver : oldver : _) -> do
            let Cabal{..} = cabal
            when (cabalVersion /= newver) $ do
                B.writeFile cabalPath $ mconcat
                    [cabalPrefix, head versions, cabalSuffix]
                run "git" ["add", cabalPath]
            readCatch "README.md" >>= \ b -> case parseReadMe oldver b of
                Left e -> die ["README.md: ", e]
                Right ReadMe{..} -> do
                    B.writeFile "README.md" $ mconcat
                        [readMePrefix, newver, readMeSuffix]
                    run "git" ["add", "README.md"]

    run "git" ["add", "CHANGELOG.md"]

    bracket (mkstemp "/tmp/wild-")
        (\ (path, _) -> removeLink path)
        $ \ (path, h) -> do
            B.hPutStr h $ mconcat ["Version: ", head versions]
            hClose h
            void $ run "git"
                ["commit", "-v", "--allow-empty-message", "-t", path]

    run "git" ["push", "origin", "master"]

    uploadResult <- uploadRelease
        token (T.encodeUtf8 owner) (T.encodeUtf8 repo) (head versions)
        binPaths
    case uploadResult of
        Left x -> error (show x)
        Right _ -> return ()

  where
    readCatch :: RawFilePath -> IO ByteString
    readCatch path = catchIOError (B.readFile path) $
        const $ B.putStrLn ("Failed reading " ++ path) *> exitFailure

runFail :: RawFilePath -> [ByteString] -> IO ByteString
runFail cmd args = do
    x <- runRead cmd args
    case x of
        Left c -> do
            B.putStrLn $ mconcat
                ["command fail: ", cmd, " (", Pack.int c, ")"]
            exitFailure
        Right output -> return output

die :: [ByteString] -> IO a
die e = B.putStrLn (mconcat e) *> exitFailure
