{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM

-- Parses the initial arrangement of Plutonian pebbles from the input file.
-- "their order is preserved" ~ poor quality Elf bait, 2024
-- >>> A.parseOnly parseRocks "1 2 3 4 5"
-- Right (fromList [(1,1),(2,1),(3,1),(4,1),(5,1)])
parseRocks :: A.Parser (IM.IntMap Int)
parseRocks = IM.fromListWith (+) . map (,1) <$> A.decimal `A.sepBy` A.char ' '

-- Determines the transformation of a single pebble based on its engraved number.
blinkStone :: Int -> IM.IntMap Int
-- "If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1"
blinkStone 0 = IM.singleton 1 1
blinkStone x =
  let digits = show x
   in if even (length digits)
        then -- "If the stone is engraved with a number that has an even number of digits, it is replaced by two stones."

          let (left, right) = join bimap (flip IM.singleton 1 . read) $ splitAt (length digits `div` 2) digits
           in -- "The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone."
              IM.unionsWith (+) [left, right]
        else -- "If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone"
          IM.singleton (x * 2024) 1

-- Applies the blink transformation to the entire arrangement of pebbles.
-- >>> blink $ IM.fromList [(125, 1), (17, 1)]
-- fromList [(1,1),(7,1),(253000,1)]
blink :: IM.IntMap Int -> IM.IntMap Int
blink = IM.unionsWith (+) . map (\(r, c) -> IM.map (* c) $ blinkStone r) . IM.toList

-- Repeats the blink transformation n times on the arrangement of pebbles.
blinkN :: Int -> IM.IntMap Int -> IM.IntMap Int
blinkN n r = iterate blink r !! n

-- Loads the initial arrangement of Plutonian pebbles from a file.
-- >>> loadInput "example.txt"
-- fromList [(17,1),(125,1)]
loadInput :: [Char] -> IO (IM.IntMap Int)
loadInput = (fromRight IM.empty . A.parseOnly parseRocks <$>) . BSC.readFile . ("src/" ++)

-- Counts the total number of pebbles in the current arrangement.
rockCount :: IM.IntMap Int -> Int
rockCount = IM.foldl (+) 0

-- Calculates the number of pebbles after 25 blinks.
-- >>> part1 <$> loadInput "example.txt"
-- 55312
part1 :: IM.IntMap Int -> Int
part1 = rockCount . blinkN 25

-- Calculates the number of pebbles after 75 blinks.
-- >>> part2 <$> loadInput "example.txt"
-- 65601038650482
part2 :: IM.IntMap Int -> Int
part2 = rockCount . blinkN 75
