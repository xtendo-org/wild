module Parser where

import ByteString (ByteString)
import qualified ByteString as B

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

class Expr expr where
    match :: expr -> Int -> ByteString -> Match

matchL1 :: L1Expr -> Int -> ByteString -> Maybe Match
matchL1 expr o b = case expr of
    ExprExact eb -> if eb `B.isPrefixOf` current
        then Just (let l = o + B.length eb in Match o l l) else Nothing
    ExprAny -> Just (let l = B.length b in Match o l l)
    ExprChain expr1 expr2 -> do
        Match _ _ c1 <- matchL1 expr1 o b
        Match _ _ c2 <- matchL1 expr2 c1 b
        return $ Match o c2 c2
    ExprWhile f -> Just $
        let l = (o +) $ B.length $ B.takeWhile f current
        in Match o l l
  where
    current = B.drop o b

-- matchL2 :: L2Expr -> ByteString -> Maybe Match
-- matchL2 expr b = case expr of
--     L2Expr expL1 -> matchL1 expL1 b
--     ExprFormer expr1 expr2 -> do
--         Match st1 ed1 c1 <- matchL2 expr1 b
--         Match _ _ c2 <- matchL2 expr2 (B.drop c1 b)
--         return $ Match st1 ed1 c2
--     ExprLatter expr1 expr2 -> do
--         Match _ _ c1 <- matchL2 expr1 b
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
