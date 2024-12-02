{-# LANGUAGE ImportQualifiedPost #-}
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

type Report = [Int]

-- >>> A.parseOnly parseReport "7 6 4 2 1"
-- Right [7,6,4,2,1]
parseReport :: A.Parser Report
parseReport = A.decimal `A.sepBy` " "

parseReports :: A.Parser [Report]
parseReports = parseReport `A.sepBy` A.endOfLine

-- >>> isSafe [7,6,4,2,1]
-- True
isSafe :: Report -> Bool
isSafe report = gradual && monotonic
  where
    gradual = all ((<= 3) . abs) intervals
    monotonic = all (> 0) intervals || all (< 0) intervals
    intervals = zipWith (-) report (tail report)

-- >>> isSafeWhenDampened [1, 3, 2, 4, 5]
-- True
isSafeWhenDampened :: Report -> Bool
isSafeWhenDampened report = isSafe report || any isSafe potentialDampens
  where
    potentialDampens = map (remove report) [0 .. length report - 1]
    remove xs i = take i xs ++ drop (i + 1) xs

-- >>> part1 <$> loadInput "example.txt"
-- 2
part1 :: [Report] -> Int
part1 = length . filter isSafe

-- >>> part2 <$> loadInput "example.txt"
-- 4
part2 :: [Report] -> Int
part2 = length . filter isSafeWhenDampened

-- >>> loadInput "example.txt"
-- [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]
loadInput :: [Char] -> IO [Report]
loadInput = (fromRight [] . A.parseOnly parseReports <$>) . BSC.readFile . ("src/" ++)
