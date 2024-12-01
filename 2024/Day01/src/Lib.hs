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

-- >>> A.parseOnly parseRow "1 2\n"
-- Right (1,2)
parseRow :: A.Parser (Int, Int)
parseRow = (,) <$> (A.decimal <* A.many1 " ") <*> A.decimal

-- >>> A.parseOnly parseRows "1 2\n3 4"
-- Right [(1,2),(3,4)]
parseRows :: A.Parser [(Int, Int)]
parseRows = sortColumns <$> parseRow `A.sepBy` A.endOfLine
  where
    sortColumns = uncurry zip . bimap sort sort . unzip

-- >>> part1 <$> loadInput "example.txt"
-- 11
part1 :: [(Int, Int)] -> Int
part1 = sum . map (abs . uncurry (-))

-- >>> part2 <$> loadInput "example.txt"
-- 31
part2 :: [(Int, Int)] -> Int
part2 tups =
  let (left, right) = unzip tups
   in sum $ mapMaybe (\x -> (x *) <$> (x `IM.lookup` elementCounts right)) left

-- >>> elementCounts [1, 2, 2, 3, 3, 3]
-- fromList [(1,1),(2,2),(3,3)]
elementCounts :: [Int] -> IM.IntMap Int
elementCounts = IM.fromListWith (+) . map (,1)

-- >>> loadInput "example.txt"
-- [(1,3),(2,3),(3,3),(3,4),(3,5),(4,9)]
loadInput :: [Char] -> IO [(Int, Int)]
loadInput = (fromRight [] . A.parseOnly parseRows <$>) . BSC.readFile . ("src/" ++)
