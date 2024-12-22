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

-- The Historian's research has uncovered the secret numbers hidden in the jungle.
-- This parser will decipher the secrets from the input file.
parseSecrets :: A.Parser [Int]
parseSecrets = A.decimal `A.sepBy` A.endOfLine

-- The secrets evolve through a series of mystical transformations.
-- This function generates the sequence of secret numbers for a given initial secret.
secrets :: Int -> [Int]
secrets = take 2001 . iterate nextSecret
  where
    mix = xor
    prune = (`mod` 16777216)
    step f s = prune (s `mix` f s)
    nextSecret = step (* 2048) . step (`div` 32) . step (* 64)

-- The Historian's notes help you load the initial secrets from a file.
-- These secrets are the key to predicting the buyers' prices.
-- >>> loadInput "example.txt"
-- [1,10,100,2024]
loadInput :: [Char] -> IO [Int]
loadInput = (fromRight [] . A.parseOnly parseSecrets <$>) . BSC.readFile . ("src/" ++)

-- To buy back the device, you need to sum the 2000th secret number for each buyer.
-- This function calculates that sum.
-- >>> part1 <$> loadInput "example.txt"
-- 37327623
part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . secrets)

-- The monkey negotiator looks for specific sequences of price changes.
-- This function prepares the price changes and their corresponding prices.
-- >>> toZippedDeltasAndPrice . take 10 $ secrets 123
-- [([-3,6,-1,-1],4),([6,-1,-1,0],4),([-1,-1,0,2],6),([-1,0,2,-2],4),([0,2,-2,0],4),([2,-2,0,-2],2)]
toZippedDeltasAndPrice :: [Int] -> [([Int], Int)]
toZippedDeltasAndPrice s =
  let prices = map (`mod` 10) s
      deltas = (zipWith (flip (-)) <*> tail) prices
      deltaGroups = map (take 4) $ tails deltas
   in zip deltaGroups (drop 4 prices)

-- To maximize the number of bananas, you need to find the best sequence of price changes.
-- This function determines the maximum number of bananas you can get.
-- >>> part2 <$> loadInput "example2.txt"
-- 23
part2 :: [Int] -> Int
part2 =
  maximum
    . M.unionsWith (+)
    . map (M.fromListWith (const id) . toZippedDeltasAndPrice . take 2000 . secrets)
