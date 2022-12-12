{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( loadInput,
    hasContainedRange,
    overlaps
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BSC
import           Data.Either                      (fromRight)
import           Data.Ix                          (inRange)

type BoundingPair = (Int, Int)

type RangePair = (BoundingPair, BoundingPair)

-- >>> (2, 9) `containedWithin` (1, 10)
-- True
containedWithin :: BoundingPair -> BoundingPair -> Bool
containedWithin (x, y) outerRange = inRange outerRange x && inRange outerRange y

-- >>> (5, 7) `overlaps` (7, 9)
-- True
overlaps :: BoundingPair -> BoundingPair -> Bool
overlaps (x, y) outerRange = inRange outerRange x || inRange outerRange y || outerRange `containedWithin` (x,y)

hasContainedRange :: BoundingPair -> BoundingPair -> Bool
hasContainedRange a b = a `containedWithin` b || b `containedWithin` a

-- >>> A.parseOnly parseRange "1-10"
-- Right (1,10)
parseRange :: A.Parser BoundingPair
parseRange = do
    start <- A.decimal <* "-"
    end <- A.decimal
    return (start, end)

-- >>> A.parseOnly parseRangePair "2-4,6-8"
-- Right ((2,4),(6,8))
parseRangePair :: A.Parser RangePair
parseRangePair = do
    first <- parseRange <* ","
    second <- parseRange
    return (first, second)

parseRangePairs :: A.Parser [RangePair]
parseRangePairs = A.sepBy1 parseRangePair A.endOfLine

loadInput :: [Char] -> IO [RangePair]
loadInput fileName =
  fromRight [] . A.parseOnly parseRangePairs <$> BSC.readFile ("src/" ++ fileName)
