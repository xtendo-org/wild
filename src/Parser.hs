module Parser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

data L1Expr
    = ExprExact ByteString
    | ExprWhile (Char -> Bool)
    | ExprAny
    | ExprChain L1Expr L1Expr

data Match = Match Int Int Int deriving (Show)

data L2Expr
    = L2Expr L1Expr
    | ExprFormer L2Expr L2Expr
    | ExprLatter L2Expr L2Expr

matchOffset :: Match -> Int -> Match
matchOffset (Match start end consumed) offset =
    Match (start + offset) (end + offset) (consumed + offset)

matchL1 :: L1Expr -> ByteString -> Maybe Match
matchL1 expr b = case expr of
    ExprExact eb -> if eb `B.isPrefixOf` b
        then Just (let l = B.length eb in Match 0 l l) else Nothing
    ExprAny -> Just (let l = B.length b in Match 0 l l)
    ExprChain expr1 expr2 -> do
        Match st1 _ c1 <- matchL1 expr1 b
        Match _ _ c2 <- matchL1 expr2 (B.drop c1 b)
        return $ let l = c1 + c2 in Match st1 l l
    ExprWhile f -> Just (let l = B.length $ B.takeWhile f b in Match 0 l l)

-- matchL2 :: L2Expr -> ByteString -> Maybe L2Match
-- matchL2 expr b = case expr of
--     L2Expr expL1 -> (\ (L1Match st ed) -> L2Match (L1Match st ed) ed) <$> matchL1 expL1 b
--     ExprFormer expr1 expr2 -> do
--         L2Match m1 c1 <- matchL2 expr1 b
--         L2Match _ c2 <- matchL2 expr2 (B.drop c1 b)
--         return $ L2Match m1 c2
--     ExprLatter expr1 expr2 -> do
--         L2Match _ c1 <- matchL2 expr1 b
--         m2 <- matchL2 expr2 (B.drop c1 b)
--         return m2

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

-- linesTill :: L1Expr -> ByteString -> Maybe L1Match
-- linesTill e b = linesTill' 0
--   where
--     linesTill' :: Int -> Maybe L1Match
--     linesTill' n = case match e current of
--         Just m -> Just $ m `matchOffset` n
--         Nothing -> if isEnd
--             then Nothing
--             else linesTill' nextIndex
--       where
--         (current, next) = B.break (== '\n') $ B.drop n b
--         isEnd = B.length next <= 1
--         nextIndex = n + B.length current + 1 -- newline character

-- parseCabal :: ByteString -> [ByteString]
-- parseCabal = parseLines $ \ b -> if "executable " `B.isPrefixOf` b
--     then Just $ B.drop (B.length "executable ") b
--     else Nothing

parseChangeLog :: ByteString -> [ByteString]
parseChangeLog = parseLines $ \ b -> if "##" `B.isPrefixOf` b
    then Just $ B.takeWhile (/= ' ') $ B.dropWhile (== ' ') $ B.drop 2 b
    else Nothing
