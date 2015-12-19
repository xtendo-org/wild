module Parser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

parseCabal :: ByteString -> [ByteString]
parseCabal b = if "executable " `B.isPrefixOf` currentLine
    then B.drop (B.length "executable ") currentLine : rest
    else rest
  where
    (currentLine, remaining_) = B.break (== '\n') b
    remaining = B.drop 1 remaining_
    rest = if B.length remaining_ <= 1 then [] else parseCabal remaining
