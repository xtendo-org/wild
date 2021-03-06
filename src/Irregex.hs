module Irregex
    ( Expr(..)
    , L1Expr(..)
    , L2Expr(..)
    , Match(..)
    , linesAll
    , linesTill
    ) where

import Data.Maybe

import ByteString (ByteString)
import qualified ByteString as B

data L1Expr
    = ExprExact ByteString
    | ExprWhile (Char -> Bool)
    | ExprScan ByteString
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
        ExprWhile f -> Just $
            let l = (o +) $ B.length $ B.takeWhile f current
            in Match o l l
        ExprScan eb -> let
            (before, after) = B.breakSubstring eb current
            spotted = o + B.length before
            consumed = spotted + B.length eb
            in if B.length after == 0 then Nothing
                else Just $ Match spotted consumed consumed
        ExprAny -> Just (let l = B.length b in Match o l l)
        ExprChain expr1 expr2 -> do
            Match _ _ c1 <- match expr1 o b
            Match _ _ c2 <- match expr2 c1 b
            return $ Match o c2 c2
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
