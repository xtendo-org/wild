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
import Unsafe

(++) :: Monoid m => m -> m -> m
(++) = mappend

data Cmd
    = CmdHelp
    | CmdPrep
    | CmdVersion
    | CmdUpload

arguments :: Mode Cmd
arguments = modes "wild" CmdHelp
    "automate releasing Haskell projects"
    [ m "prep" CmdPrep "build project and strip executables\
        \ to prepare for release"
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
    arg <- processArgs arguments
    case arg of
        CmdHelp     -> print $ helpText [] HelpFormatDefault arguments
        CmdPrep     -> runPrep
        CmdVersion  -> runVersion
        CmdUpload   -> runUpload

runPrep :: IO ()
runPrep = do
    Cabal{..} <- getCabal
    run "stack" ["install"]
    forM_ cabalExecs $ \ bin -> do
        run "strip"
            [ "--strip-all"
            , "--remove-section=.comment", "--remove-section=.note"
            , bin
            ]
        run "upx" ["-9", bin]

runVersion :: IO ()
runVersion = do
    versions <- parseChangeLog <$> readCatch "CHANGELOG.md"
    case versions of
        [] -> die ["version not found in change log"]
        [_] -> return ()
        (newver : oldver : _) -> do
            cabalPath <- getCabalPath
            Cabal{..} <- getCabal
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

runUpload :: IO ()
runUpload = do
    versions <- parseChangeLog <$> readCatch "CHANGELOG.md"
    homePath <- getHomePath
    Cabal{..} <- getCabal
    Conf{..} <- fmap (either error id . dec) $
        catchIOError (B.readFile "wild.json") $ const $ do
            B.writeFile "wild.json" $ enc initConf
            B.putStrLn "Created wild.json with default configuration."
            exitFailure

    token <- readCatch $ let
        path = T.encodeUtf8 tokenPath
        in if B.head path == '~' then homePath ++ B.tail path else path

    uploadResult <- uploadRelease
        token (T.encodeUtf8 owner) (T.encodeUtf8 repo) (head versions)
        cabalExecs
    case uploadResult of
        Left x -> error (show x)
        Right _ -> return ()

getHomePath :: IO RawFilePath
getHomePath = getRef homePathRef $ getEnv "HOME" >>= \ m -> case m of
    Nothing -> die ["No $HOME?!"]
    Just p -> return p

getCabalPath :: IO RawFilePath
getCabalPath = getRef cabalPathRef $
    getDirectoryContentsSuffix "." ".cabal" >>= \ s -> case s of
        [] -> error "no cabal file found in the current directory"
        (x : _) -> return x

getCabal :: IO Cabal
getCabal = getRef cabalRef $ do
    homePath <- getHomePath
    cabalPath <- getCabalPath
    let binPath = map (homePath ++ "/.local/bin/" ++)
    parseCabal <$> readCatch cabalPath  >>= \ m -> case m of
        Left e -> die [e]
        Right c -> return c { cabalExecs = binPath (cabalExecs c) }

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

readCatch :: RawFilePath -> IO ByteString
readCatch path = catchIOError (B.readFile path) $
    const $ B.putStrLn ("Failed reading " ++ path) *> exitFailure
