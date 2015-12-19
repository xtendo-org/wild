module Parser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

data L1Expr
    = ExprExact ByteString
    | ExprWhile (Char -> Bool)
    | ExprAny
    | ExprChain L1Expr L1Expr

data L1Match = L1Match Int Int
    deriving (Show)

matchOffset :: L1Match -> Int -> L1Match
matchOffset (L1Match start end) offset =
    L1Match (start + offset) (end + offset)

match :: L1Expr -> ByteString -> Maybe L1Match
match e b = case e of
    ExprExact eb -> if eb `B.isPrefixOf` b
        then Just (L1Match 0 $ B.length eb) else Nothing
    ExprAny -> Just (L1Match 0 $ B.length b)
    ExprChain exp1 exp2 -> case match exp1 b of
        Just (L1Match start1 end1) -> case match exp2 (B.drop end1 b) of
            Just (L1Match _ end2) -> Just (L1Match start1 (end1 + end2))
            Nothing -> Nothing
        Nothing -> Nothing
    ExprWhile f -> Just (L1Match 0 (B.length $ B.takeWhile f b))

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

linesTill :: L1Expr -> ByteString -> Maybe L1Match
linesTill e b = linesTill' 0
  where
    linesTill' :: Int -> Maybe L1Match
    linesTill' n = case match e current of
        Just m -> Just $ m `matchOffset` n
        Nothing -> if isEnd
            then Nothing
            else linesTill' nextIndex
      where
        (current, next) = B.break (== '\n') $ B.drop n b
        isEnd = B.length next <= 1
        nextIndex = n + B.length current + 1 -- newline character

-- parseCabal :: ByteString -> [ByteString]
-- parseCabal = parseLines $ \ b -> if "executable " `B.isPrefixOf` b
--     then Just $ B.drop (B.length "executable ") b
--     else Nothing

parseChangeLog :: ByteString -> [ByteString]
parseChangeLog = parseLines $ \ b -> if "##" `B.isPrefixOf` b
    then Just $ B.takeWhile (/= ' ') $ B.dropWhile (== ' ') $ B.drop 2 b
    else Nothing
