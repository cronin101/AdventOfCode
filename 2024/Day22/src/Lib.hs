{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( part1,
    part2,
    loadInput,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bits (xor)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (tails)
import Data.Map qualified as M

parseSecrets :: A.Parser [Int]
parseSecrets = A.decimal `A.sepBy` A.endOfLine

secrets :: Int -> [Int]
secrets = take 2001 . iterate nextSecret
  where
    mix = xor
    prune = (`mod` 16777216)
    step f s = prune (s `mix` f s)
    nextSecret = step (* 2048) . step (`div` 32) . step (* 64)

-- >>> loadInput "example.txt"
-- [1,10,100,2024]
loadInput :: [Char] -> IO [Int]
loadInput = (fromRight [] . A.parseOnly parseSecrets <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 37327623
part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . secrets)

-- >>> toZippedDeltasAndPrice . take 10 $ secrets 123
-- [([-3,6,-1,-1],4),([6,-1,-1,0],4),([-1,-1,0,2],6),([-1,0,2,-2],4),([0,2,-2,0],4),([2,-2,0,-2],2)]
toZippedDeltasAndPrice :: [Int] -> [([Int], Int)]
toZippedDeltasAndPrice s =
  let prices = map (`mod` 10) s
      deltas = (zipWith (flip (-)) <*> tail) prices
      deltaGroups = map (take 4) $ tails deltas
   in zip deltaGroups (drop 4 prices)

-- >>> part2 <$> loadInput "example2.txt"
-- 23
part2 :: [Int] -> Int
part2 =
  maximum
    . M.unionsWith (+)
    . map (M.fromListWith (const id) . toZippedDeltasAndPrice . take 2000 . secrets)
