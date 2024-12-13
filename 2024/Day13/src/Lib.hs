{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)

data Button where
  Button :: {offset' :: (Int, Int)} -> Button
  deriving (Show)

data Machine = Machine {buttons' :: (Button, Button), prize' :: (Int, Int)}
  deriving (Show)

type WinningPresses = (Int, Int)

-- Parses the button configuration from the input string.
-- Each button has an offset that determines how much it moves the claw.
-- >>> A.parseOnly parseButton "Button A: X+94, Y+34"
-- Right (Button {label' = 'A', offset' = (94,34)})
parseButton :: A.Parser Button
parseButton = "Button " *> A.anyChar *> ": " *> (Button <$> parseOffset)
  where
    parseOffset = (,) <$> ("X" *> A.signed A.decimal) <*> (", Y" *> A.signed A.decimal)

-- Parses the machine configuration, including both buttons and the prize location.
parseMachine :: A.Parser Machine
parseMachine = do
  buttons <- (,) <$> (parseButton <* A.endOfLine) <*> (parseButton <* A.endOfLine)
  offset <- (,) <$> ("Prize: X=" *> A.decimal) <*> (", Y=" *> A.decimal)
  return $ Machine buttons offset

-- Parses multiple machines from the input.
parseMachines :: A.Parser [Machine]
parseMachines = parseMachine `A.sepBy` A.count 2 A.endOfLine

-- Moves the claw by the specified delta.
-- This function calculates the new position of the claw after pressing a button.
moveClaw :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveClaw p delta = bimap (+ fst delta) (+ snd delta) p

-- Determines the number of presses needed to align the claw with the prize.
-- If the claw can be aligned with the prize using the given button presses, it returns the number of presses.
pressesNeeded :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe Int
pressesNeeded target (bx, by) current =
  let (dx, dy) = bimap (subtract $ fst current) (subtract $ snd current) target
   in if dx `mod` bx == 0 && dy `mod` by == 0
        then case (dx `div` bx, dy `div` by) of
          (x, y) | x == y -> Just x
          _ -> Nothing
        else Nothing

-- Loads the input file and parses it into a list of machines.
-- >>> loadInput "example.txt"
-- [Machine {buttons' = (Button {offset' = (94,34)},Button {offset' = (22,67)}), prize' = (8400,5400)},Machine {buttons' = (Button {offset' = (26,66)},Button {offset' = (67,21)}), prize' = (12748,12176)},Machine {buttons' = (Button {offset' = (17,86)},Button {offset' = (84,37)}), prize' = (7870,6450)},Machine {buttons' = (Button {offset' = (69,23)},Button {offset' = (27,71)}), prize' = (18641,10279)}]
loadInput :: [Char] -> IO [Machine]
loadInput = (fromRight [] . A.parseOnly parseMachines <$>) . BSC.readFile . ("src/" ++)

-- Iteratively calculates all possible winning presses for a machine.
-- This function explores different combinations of button presses to find the ones that align the claw with the prize.
-- >>> map iterativeAllWins <$> loadInput "example.txt"
-- [[(80,40)],[],[(38,86)],[]]
iterativeAllWins :: Machine -> [WinningPresses]
iterativeAllWins (Machine (Button firstDelta, Button secondDelta) target) =
  let origin = (0, 0)
      maxPresses = 100
      presses = [(a, b) | (a, Just b) <- zipWith (curry (second $ pressesNeeded target secondDelta)) [1 .. maxPresses] (drop 1 $ iterate (`moveClaw` firstDelta) origin)]
   in -- firstPresses = [(a, b) | (a, b) <- zipWith (curry (second (pressesNeeded target secondDelta))) [1 .. maxPresses] (drop 1 $ iterate (`moveClaw` firstDelta) origin)]
      presses

-- Calculates the cost in tokens to win a prize with the given number of presses.
cost :: WinningPresses -> Int
cost (a, b) = 3 * a + b

-- Calculates the total cost to win all possible prizes.
-- This function sums up the minimum cost for each machine to win a prize.
costToWinAllPossible :: [[WinningPresses]] -> Int
costToWinAllPossible = sum . map (minimum . map cost) . filter (not . null)

-- Calculates the total cost to win all possible prizes for part 1 of the puzzle.
-- >>> part1 <$> loadInput "example.txt"
-- 480
part1 :: [Machine] -> Int
part1 = costToWinAllPossible . map iterativeAllWins

-- Analytically calculates all possible winning presses for a machine after adjusting the prize position.
-- This function uses Cramer's rule to find the exact number of presses needed to align the claw with the prize.
analyticalALlWins :: Machine -> [WinningPresses]
analyticalALlWins m = maybe [] pure (cramersRule m)

-- Calculates the determinant of the matrix formed by the button offsets.
-- This function checks if the button offsets are linearly independent.
determinant :: Machine -> Int
determinant (Machine (Button (x, y), Button (x', y')) _) = x * y' - y * x'

-- Checks if the button offsets are linearly independent.
-- If the determinant is non-zero, the button offsets are linearly independent.
-- >>> map isLinearlyIndependent <$> loadInput "example.txt"
-- [True,True,True,True]
isLinearlyIndependent :: Machine -> Bool
isLinearlyIndependent = (/= 0) . determinant

-- Uses Cramer's rule to calculate the exact number of presses needed to align the claw with the prize.
-- If the button offsets are linearly independent, this function returns the number of presses needed.
-- "In linear algebra, Cramer's rule is an explicit formula for the solution of a system of linear equations
-- with as many equations as unknowns, valid whenever the system has a unique solution."
-- ~ https://en.wikipedia.org/wiki/Cramer%27s_rule
-- >>> map cramersRule <$> loadInput "example.txt"
-- [Just (80,40),Nothing,Just (38,86),Nothing]
cramersRule :: Machine -> Maybe (Int, Int)
cramersRule m@(Machine (Button (x, y), Button (x', y')) (tx, ty))
  | isLinearlyIndependent m =
      let d = determinant m
          numeratorA = (tx * y' - ty * x')
          numeratorB = ((-tx) * y + ty * x)
       in case (numeratorA `mod` d, numeratorB `mod` d) of
            (0, 0) -> Just (numeratorA `div` d, numeratorB `div` d)
            _ -> Nothing
  | otherwise = Nothing

-- Calculates the total cost to win all possible prizes for part 2 of the puzzle.
-- >>> part2 <$> loadInput "example.txt"
-- 875318608908
part2 :: [Machine] -> Int
part2 = costToWinAllPossible . map (analyticalALlWins . adjustPrize)
  where
    adjustPrize m = m {prize' = join bimap (10000000000000 +) $ prize' m}
