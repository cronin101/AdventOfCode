{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM
import Data.List (sort)
import Data.Maybe (mapMaybe)

-- The Chief Historian's notes are cryptic, but with a bit of parsing magic, we can decipher the pairs of location IDs.
-- >>> A.parseOnly parseRow "1 2\n"
-- Right (1,2)
parseRow :: A.Parser (Int, Int)
parseRow = (,) <$> (A.decimal <* A.many1 " ") <*> A.decimal

-- The notes are scattered across multiple lines. Let's gather all pairs of location IDs and sort them for easier comparison.
-- >>> A.parseOnly parseRows "1 2\n3 4"
-- Right [(1,2),(3,4)]
parseRows :: A.Parser [(Int, Int)]
parseRows = sortColumns <$> parseRow `A.sepBy` A.endOfLine
  where
    sortColumns = uncurry zip . bimap sort sort . unzip

-- The Historians need to measure the total distance between their lists of location IDs to find out how far apart they are.
-- >>> part1 <$> loadInput "example.txt"
-- 11
part1 :: [(Int, Int)] -> Int
part1 = sum . map (abs . uncurry (-))

-- The Historians suspect that some location IDs appear in both lists. Let's calculate the similarity score based on these common IDs.
-- >>> part2 <$> loadInput "example.txt"
-- 31
part2 :: [(Int, Int)] -> Int
part2 tups =
  let (left, right) = unzip tups
   in sum $ mapMaybe (\x -> (x *) <$> (x `IM.lookup` elementCounts right)) left

-- To help the Historians, we need to count how many times each location ID appears in the right list.
-- >>> elementCounts [1, 2, 2, 3, 3, 3]
-- fromList [(1,1),(2,2),(3,3)]
elementCounts :: [Int] -> IM.IntMap Int
elementCounts = IM.fromListWith (+) . map (,1)

-- The Chief Historian's notes are stored in a file. Let's load and parse them to get the list of location ID pairs.
-- >>> loadInput "example.txt"
-- [(1,3),(2,3),(3,3),(3,4),(3,5),(4,9)]
loadInput :: [Char] -> IO [(Int, Int)]
loadInput = (fromRight [] . A.parseOnly parseRows <$>) . BSC.readFile . ("src/" ++)
