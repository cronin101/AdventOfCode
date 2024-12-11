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
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM

-- "their order is preserved" ~ poor quality Elf bait, 2024
-- >>> A.parseOnly parseRocks "1 2 3 4 5"
-- Right (fromList [(1,1),(2,1),(3,1),(4,1),(5,1)])
parseRocks :: A.Parser (IM.IntMap Int)
parseRocks = IM.fromListWith (+) . map (,1) <$> A.decimal `A.sepBy` A.char ' '

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone x =
  let digits = show x
   in if even (length digits)
        then (\(l, r) -> [read l, read r]) (splitAt (length digits `div` 2) digits)
        else [x * 2024]

-- >>> blink [125, 17]
-- [253000,1,7]
blink :: IM.IntMap Int -> IM.IntMap Int
blink = IM.fromListWith (+) . concatMap blinkCount . IM.toList
  where
    blinkCount :: (Int, Int) -> [(Int, Int)]
    blinkCount (r, c) = map (,c) $ blinkStone r

blinkN :: Int -> IM.IntMap Int -> IM.IntMap Int
blinkN n r = iterate blink r !! n

-- >>> loadInput "example.txt"
-- [125,17]
loadInput :: [Char] -> IO (IM.IntMap Int)
loadInput = (fromRight IM.empty . A.parseOnly parseRocks <$>) . BSC.readFile . ("src/" ++)

rockCount :: IM.IntMap Int -> Int
rockCount = IM.foldl' (+) 0

-- >>> part1 <$> loadInput "example.txt"
-- 55312
part1 :: IM.IntMap Int -> Int
part1 = rockCount . blinkN 25

-- >>> part2 <$> loadInput "input.txt"
-- 237149922829154
part2 :: IM.IntMap Int -> Int
part2 = rockCount . blinkN 75
