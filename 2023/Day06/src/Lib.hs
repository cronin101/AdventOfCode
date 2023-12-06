{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1',
    part2',
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (sort)

-- (Time, NeededDistance)
type Challenge = (Int, Int)

data ChallengeSolution = ChallengeSolution {holdTime :: Int, traveled :: Int} deriving (Show)

-- >>> A.parseOnly parseChallenges "Time:      7  15   30\nDistance:  9  40  200"
-- Right [(7,9),(15,40),(30,200)]
parseChallenges :: A.Parser [Challenge]
parseChallenges = do
  times <- "Time:" *> A.skipSpace *> A.sepBy1' A.decimal A.skipSpace <* A.endOfLine
  distances <- "Distance:" *> A.skipSpace *> A.sepBy1' A.decimal A.skipSpace
  return $ zip times distances

-- >>> loadInput "example.txt"
-- [(7,9),(15,40),(30,200)]
loadInput :: [Char] -> IO [Challenge]
loadInput = (fromRight [] . A.parseOnly parseChallenges <$>) . BSC.readFile . ("src/" ++)

-- Brute force generating all solutions
-- >>> map solutions <$> loadInput "example.txt"
-- [[ChallengeSolution {holdTime = 2, traveled = 10},ChallengeSolution {holdTime = 3, traveled = 12},ChallengeSolution {holdTime = 4, traveled = 12},ChallengeSolution {holdTime = 5, traveled = 10}],[ChallengeSolution {holdTime = 4, traveled = 44},ChallengeSolution {holdTime = 5, traveled = 50},ChallengeSolution {holdTime = 6, traveled = 54},ChallengeSolution {holdTime = 7, traveled = 56},ChallengeSolution {holdTime = 8, traveled = 56},ChallengeSolution {holdTime = 9, traveled = 54},ChallengeSolution {holdTime = 10, traveled = 50},ChallengeSolution {holdTime = 11, traveled = 44}],[ChallengeSolution {holdTime = 11, traveled = 209},ChallengeSolution {holdTime = 12, traveled = 216},ChallengeSolution {holdTime = 13, traveled = 221},ChallengeSolution {holdTime = 14, traveled = 224},ChallengeSolution {holdTime = 15, traveled = 225},ChallengeSolution {holdTime = 16, traveled = 224},ChallengeSolution {holdTime = 17, traveled = 221},ChallengeSolution {holdTime = 18, traveled = 216},ChallengeSolution {holdTime = 19, traveled = 209}]]
solutions :: Challenge -> [ChallengeSolution]
solutions (time, distanceNeeded) = filter ((> distanceNeeded) . traveled) $ map (\t -> ChallengeSolution t ((time - t) * t)) [1 .. time - 1]

-- Using the quadratic formula to solve the equation
-- >>> solveQuadratic (30, 200)
-- (11,19)
solveQuadratic :: Challenge -> (Int, Int)
solveQuadratic (time, distanceNeeded) = (ceiling lo, floor hi)
  where
    [lo, hi] = sort $ map (\op -> (-b) `op` sqrt bSquaredMinus4AC / (2 * a)) [(+), (-)]
    a = -1
    b = fromIntegral time
    c = fromIntegral (-(distanceNeeded + 1))
    bSquaredMinus4AC = (b ^ 2) - 4 * a * c

-- Analytical approach to the number of solutions
solutionCount :: Challenge -> Int
solutionCount c = (hi - lo) + 1
  where
    (lo, hi) = solveQuadratic c

-- Numerical solution
-- >>> part1 <$> loadInput "example.txt"
-- 288
part1 :: [Challenge] -> Int
part1 = product . map (length . solutions)

-- Analytical solution
-- >>> part1' <$> loadInput "example.txt"
-- 288
part1' :: [Challenge] -> Int
part1' = product . map solutionCount

-- Numerical solution
-- >>> part2 <$> loadInput "example.txt"
-- 71503
part2 :: [Challenge] -> Int
part2 cs = length $ solutions c
  where
    c = (read $ concatMap (show . fst) cs, read $ concatMap (show . snd) cs)

-- Analytical solution
-- >>> part2' <$> loadInput "example.txt"
-- 71503
part2' cs = solutionCount c
  where
    c = (read $ concatMap (show . fst) cs, read $ concatMap (show . snd) cs)
