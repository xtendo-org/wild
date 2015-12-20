module Parser where

import ByteString (ByteString)
import qualified ByteString as B

import Irregex

data Cabal = Cabal
    { cabalPrefix :: ByteString
    , cabalVersion :: ByteString
    , cabalSuffix :: ByteString
    , cabalExecs :: [ByteString]
    }

parseLines :: (ByteString -> Maybe a) -> ByteString -> [a]
parseLines f b = this rest
  where
    (currentLine, remaining_) = B.break (== '\n') b
    remaining = B.drop 1 remaining_
    rest = if B.length remaining_ <= 1 then [] else parseLines f remaining
    this = case f currentLine of
        Just x -> (x :)
        _ -> id

parseCabal :: ByteString -> Either ByteString Cabal
parseCabal b = case linesTill versionExpr b of
    Nothing -> Left "can't find the version field in the cabal file"
    Just (Match st ed _) -> let suffix = B.drop ed b in Right Cabal
        { cabalPrefix = B.take st b
        , cabalVersion = B.range st ed b
        , cabalSuffix = suffix
        , cabalExecs = (\ (Match st1 ed1 _) -> B.range st1 ed1 suffix) <$>
            linesAll executableExpr suffix
        }
  where
    versionExpr =
        L1Expr (ExprExact "version:" `ExprChain` ExprWhile (' ' ==))
        `ExprLatter`
        L1Expr ExprAny
    executableExpr =
        L1Expr (ExprExact "executable " `ExprChain` ExprWhile (' ' ==))
        `ExprLatter`
        L1Expr ExprAny

parseChangeLog :: ByteString -> [ByteString]
parseChangeLog = parseLines $ \ b -> if "##" `B.isPrefixOf` b
    then Just $ B.takeWhile (/= ' ') $ B.dropWhile (== ' ') $ B.drop 2 b
    else Nothing

data ReadMe = ReadMe
    { readMePrefix :: ByteString
    , readMeSuffix :: ByteString
    }

parseReadMe :: ByteString -> ByteString -> Either ByteString ReadMe
parseReadMe ver b = case match (ExprScan ver) 0 b of
    Nothing -> Left $ mconcat ["version string ", ver, " not found"]
    Just (Match st ed _) -> Right ReadMe
        { readMePrefix = B.take st b
        , readMeSuffix = B.drop ed b
        }
