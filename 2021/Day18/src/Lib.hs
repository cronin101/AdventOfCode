{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  (
  )
where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit)
import qualified Data.Char as A
import Data.Either (fromRight, isRight)
import Data.Maybe (fromJust, listToMaybe)
import Data.Text
import Data.Text.Internal.Search
import Numeric (readInt)

type LiteralPair = (Int, Int)

data Pair = Literal LiteralPair | Balanced (Pair, Pair) | LeftPair (Pair, Int) | RightPair (Int, Pair)
  deriving (Show, Eq)

{-
instance Show Pair where
  show (Literal (x, y)) = show [x, y]
  show (LeftPair (l, i)) = "[" ++ show l ++ "," ++ show i ++ "]"
  show (RightPair (i, r)) = "[" ++ show i ++ "," ++ show r ++ "]"
  show (Balanced (l, r)) = "[" ++ show l ++ "," ++ show r ++ "]"
-}

-- >>> A.parseOnly parseLiteralPair "[1,2]"
-- Right [1,2]
parseLiteralPair :: A.Parser Pair
parseLiteralPair = do
  "["
  left <- digitToInt <$> A.digit
  ","
  right <- digitToInt <$> A.digit
  "]"
  return $ Literal (left, right)

-- >>> A.parseOnly parsePair "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
-- Right [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
parsePair :: A.Parser Pair
parsePair = parseLiteralPair <|> parseBalancedPair <|> parseLeftPair <|> parseRightPair

-- >>> A.parseOnly parseBalancedPair "[[1,9],[8,5]]"
-- Right [[1,9],[8,5]]
parseBalancedPair :: A.Parser Pair
parseBalancedPair = do
  "["
  left <- parsePair
  ","
  right <- parsePair
  "]"
  return $ Balanced (left, right)

-- >>> A.parseOnly parseLeftPair "[[1,2],3]"
-- Right [[1,2],3]
parseLeftPair :: A.Parser Pair
parseLeftPair = do
  "["
  left <- parsePair
  ","
  right <- digitToInt <$> A.digit
  "]"
  return $ LeftPair (left, right)

--- >>> A.parseOnly parseRightPair "[9,[8,7]]"
-- Right [9,[8,7]]
parseRightPair :: A.Parser Pair
parseRightPair = do
  "["
  left <- digitToInt <$> A.digit
  ","
  right <- parsePair
  "]"
  return $ RightPair (left, right)

-- >>> readPair "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
-- [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
readPair :: BSC.ByteString -> Pair
readPair input = result
  where
    Right result = A.parseOnly parsePair input

-- >>> readPair "[1,2]" `add` readPair "[[3,4],5]"
-- [[1,2],[[3,4],5]]
add :: Pair -> Pair -> Pair
add left right = Balanced (left, right)

-- >>> explode $ readPair "[[[[[9,8],1],2],3],4]"
-- [[[[0,9],2],3],4]
explode :: Pair -> Pair
explode (LeftPair (LeftPair (LeftPair (LeftPair (Literal (_, right), target), a), b), c)) = LeftPair (LeftPair (LeftPair (Literal (0, target + right), a), b), c)
explode (RightPair (a, RightPair (b, RightPair (c, RightPair (target, Literal (left, _)))))) = RightPair (a, RightPair (b, RightPair (c, Literal (target + left, 0))))
explode (Literal l) = Literal l
explode (LeftPair (p, r)) = LeftPair (explode p, r)
explode (RightPair (l, p)) = RightPair (l, explode p)
explode (Balanced (a, b)) = Balanced (explode a, explode b)

-- >>> explode $ readPair "[7,[6,[5,[4,[3,2]]]]]"
-- [7,[6,[5,[7,0]]]]

-- >>> explode $ readPair "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
-- No instance for (Show Pair) arising from a use of ‘evalPrint’
