module Parser where

import Data.Maybe

import ByteString (ByteString)
import qualified ByteString as B

breakOff :: Int -> (Char -> Bool) -> ByteString -> Int
breakOff offset f b = offset + B.length (B.takeWhile f $ B.drop offset b)

data L1Expr
    = ExprExact ByteString
    | ExprWhile (Char -> Bool)
    | ExprAny
    | ExprChain L1Expr L1Expr

data Match = Match Int Int Int deriving (Show)

data L2Expr
    = L1Expr L1Expr
    | ExprFormer L2Expr L2Expr
    | ExprLatter L2Expr L2Expr

class Expr expr where
    match :: expr -> Int -> ByteString -> Maybe Match

instance Expr L1Expr where
    match expr o b = case expr of
        ExprExact eb -> if eb `B.isPrefixOf` current
            then Just (let l = o + B.length eb in Match o l l) else Nothing
        ExprAny -> Just (let l = B.length b in Match o l l)
        ExprChain expr1 expr2 -> do
            Match _ _ c1 <- match expr1 o b
            Match _ _ c2 <- match expr2 c1 b
            return $ Match o c2 c2
        ExprWhile f -> Just $
            let l = (o +) $ B.length $ B.takeWhile f current
            in Match o l l
      where
        current = B.drop o b

instance Expr L2Expr where
    match expr o b = case expr of
        L1Expr exprL1 -> match exprL1 o b
        ExprFormer expr1 expr2 -> do
            Match st1 ed1 c1 <- match expr1 o b
            Match _ _ c2 <- match expr2 c1 b
            return $ Match st1 ed1 c2
        ExprLatter expr1 expr2 -> do
            Match _ _ c1 <- match expr1 o b
            match expr2 c1 b

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

linesAll :: Expr expr => expr -> ByteString -> [Match]
linesAll expr b = linesAll' 0
  where
    linesAll' :: Int -> [Match]
    linesAll' n = case match expr n (B.take nextIndex b) of
        Just m -> m : rest
        Nothing -> if isEnd then [] else rest
      where
        rest = linesAll' (nextIndex + 1)
        (current, next) = B.break (== '\n') $ B.drop n b
        isEnd = B.length next <= 1
        nextIndex = n + B.length current

linesTill :: Expr expr => expr -> ByteString -> Maybe Match
linesTill expr b = listToMaybe (linesAll expr b)

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

-- parseCabal :: ByteString -> [ByteString]
-- parseCabal = parseLines $ \ b -> if "executable " `B.isPrefixOf` b
--     then Just $ B.drop (B.length "executable ") b
--     else Nothing

parseChangeLog :: ByteString -> [ByteString]
parseChangeLog = parseLines $ \ b -> if "##" `B.isPrefixOf` b
    then Just $ B.takeWhile (/= ' ') $ B.dropWhile (== ' ') $ B.drop 2 b
    else Nothing
