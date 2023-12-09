{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)

-- >>> A.parseOnly parseSequence "-2 -6 -8 10 86 287 734 1656 3486 7012 13596 25474 46150 80897 137378 226400 362814 566574 863968 1289034 1885174"
-- Right [-2,-6,-8,10,86,287,734,1656,3486,7012,13596,25474,46150,80897,137378,226400,362814,566574,863968,1289034,1885174]
parseSequence :: A.Parser [Int]
parseSequence = A.signed A.decimal `A.sepBy` " "

-- >>> loadInput "example.txt"
-- [[0,3,6,9,12,15],[1,3,6,10,15,21],[10,13,16,21,30,45]]
loadInput :: [Char] -> IO [[Int]]
loadInput = (fromRight [] . A.parseOnly (parseSequence `A.sepBy1` A.endOfLine) <$>) . BSC.readFile . ("src/" ++)

sequenceToDeltas :: [Int] -> [Int]
sequenceToDeltas s = zipWith subtract s (drop 1 s)

deltas :: [Int] -> [[Int]]
deltas = reverse . takeWhile (any (/= 0)) . iterate sequenceToDeltas

calculateTerm :: ([Int] -> Int) -> (Int -> Int -> Int) -> [Int] -> Int
calculateTerm deltaPicker combineDeltas =
  head
    . head
    . dropWhile ((> 1) . length)
    . iterate (\case (x : y : xs) -> (y `combineDeltas` x) : xs; xs -> xs)
    . map deltaPicker
    . deltas

nextTerm :: [Int] -> Int
nextTerm = calculateTerm last (+)

previousTerm :: [Int] -> Int
previousTerm = calculateTerm head (-)

-- >>> part1 <$> loadInput "example.txt"
-- 114
part1 :: [[Int]] -> Int
part1 = sum . map nextTerm

-- >>> part2 <$> loadInput "example.txt"
-- 2
part2 :: [[Int]] -> Int
part2 = sum . map previousTerm
