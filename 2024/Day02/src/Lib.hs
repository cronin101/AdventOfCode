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

-- The engineers have provided a parser to read the unusual data from the reactor.
-- This parser will interpret each report as a list of levels separated by spaces.
-- >>> A.parseOnly parseReport "7 6 4 2 1"
-- Right [7,6,4,2,1]
parseReport :: A.Parser Report
parseReport = A.decimal `A.sepBy` " "

-- The engineers have provided a parser to read multiple reports from the reactor.
-- Each report is separated by a newline.
parseReports :: A.Parser [Report]
parseReports = parseReport `A.sepBy` A.endOfLine

-- The engineers need to determine if a report is safe.
-- A report is considered safe if the levels are either all increasing or all decreasing,
-- and any two adjacent levels differ by at least one and at most three.
-- >>> isSafe [7,6,4,2,1]
-- True
isSafe :: Report -> Bool
isSafe report = gradual && monotonic
  where
    gradual = all ((<= 3) . abs) intervals
    monotonic = all (> 0) intervals || all (< 0) intervals
    intervals = zipWith (-) report (tail report)

-- The engineers have introduced the Problem Dampener.
-- This function checks if a report is safe when the Problem Dampener is applied.
-- The Problem Dampener allows the reactor safety systems to tolerate a single bad level.
-- >>> isSafeWhenDampened [1, 3, 2, 4, 5]
-- True
isSafeWhenDampened :: Report -> Bool
isSafeWhenDampened report = isSafe report || any isSafe potentialDampens
  where
    potentialDampens = map (remove report) [0 .. length report - 1]
    remove xs i = take i xs ++ drop (i + 1) xs

-- The engineers need to count how many reports are safe without the Problem Dampener.
-- This function will analyze the unusual data and return the count of safe reports.
-- >>> part1 <$> loadInput "example.txt"
-- 2
part1 :: [Report] -> Int
part1 = length . filter isSafe

-- The engineers need to count how many reports are safe with the Problem Dampener.
-- This function will analyze the unusual data and return the count of safe reports
-- when the Problem Dampener is applied.
-- >>> part2 <$> loadInput "example.txt"
-- 4
part2 :: [Report] -> Int
part2 = length . filter isSafeWhenDampened

-- The engineers have provided a function to load the unusual data from a file.
-- This function reads the file and parses the reports.
-- >>> loadInput "example.txt"
-- [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]
loadInput :: [Char] -> IO [Report]
loadInput = (fromRight [] . A.parseOnly parseReports <$>) . BSC.readFile . ("src/" ++)
