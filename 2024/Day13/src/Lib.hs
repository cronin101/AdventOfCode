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

data Button = Button {label' :: Char, offset' :: (Int, Int)}
  deriving (Show)

data Machine = Machine {buttons' :: (Button, Button), prize' :: (Int, Int)}
  deriving (Show)

type WinningPresses = (Int, Int)

-- >>> A.parseOnly parseButton "Button A: X+94, Y+34"
-- Right (Button {label' = 'A', offset' = (94,34)})
parseButton :: A.Parser Button
parseButton = "Button " *> (Button <$> A.anyChar <* ": " <*> parseOffset)
  where
    parseOffset = (,) <$> ("X" *> A.signed A.decimal) <*> (", Y" *> A.signed A.decimal)

parseMachine :: A.Parser Machine
parseMachine = do
  buttons <- (,) <$> (parseButton <* A.endOfLine) <*> (parseButton <* A.endOfLine)
  offset <- (,) <$> ("Prize: X=" *> A.decimal) <*> (", Y=" *> A.decimal)
  return $ Machine buttons offset

parseMachines :: A.Parser [Machine]
parseMachines = parseMachine `A.sepBy` A.count 2 A.endOfLine

moveClaw :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveClaw p delta = bimap (+ fst delta) (+ snd delta) p

pressesNeeded :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe Int
pressesNeeded target (bx, by) current =
  let (dx, dy) = bimap (subtract $ fst current) (subtract $ snd current) target
   in if dx `mod` bx == 0 && dy `mod` by == 0
        then case (dx `div` bx, dy `div` by) of
          (x, y) | x == y -> Just x
          _ -> Nothing
        else Nothing

-- >>> loadInput "example.txt"
-- [Machine {buttons' = (Button {label' = 'A', offset' = (94,34)},Button {label' = 'B', offset' = (22,67)}), prize' = (8400,5400)},Machine {buttons' = (Button {label' = 'A', offset' = (26,66)},Button {label' = 'B', offset' = (67,21)}), prize' = (12748,12176)},Machine {buttons' = (Button {label' = 'A', offset' = (17,86)},Button {label' = 'B', offset' = (84,37)}), prize' = (7870,6450)},Machine {buttons' = (Button {label' = 'A', offset' = (69,23)},Button {label' = 'B', offset' = (27,71)}), prize' = (18641,10279)}]
loadInput :: [Char] -> IO [Machine]
loadInput = (fromRight [] . A.parseOnly parseMachines <$>) . BSC.readFile . ("src/" ++)

-- >>> map iterativeAllWins <$> loadInput "example.txt"
-- [[(80,40)],[],[(38,86)],[]]
iterativeAllWins :: Machine -> [WinningPresses]
iterativeAllWins (Machine (Button _ firstDelta, Button _ secondDelta) target) =
  let origin = (0, 0)
      maxPresses = 100
      presses = [(a, b) | (a, Just b) <- zipWith (curry (second $ pressesNeeded target secondDelta)) [1 .. maxPresses] (drop 1 $ iterate (`moveClaw` firstDelta) origin)]
   in -- firstPresses = [(a, b) | (a, b) <- zipWith (curry (second (pressesNeeded target secondDelta))) [1 .. maxPresses] (drop 1 $ iterate (`moveClaw` firstDelta) origin)]
      presses

cost :: WinningPresses -> Int
cost (a, b) = 3 * a + b

costToWinAllPossible :: [[WinningPresses]] -> Int
costToWinAllPossible = sum . map (minimum . map cost) . filter (not . null)

-- >>> part1 <$> loadInput "example.txt"
-- 480
part1 :: [Machine] -> Int
part1 = costToWinAllPossible . map iterativeAllWins

analyticalALlWins :: Machine -> [WinningPresses]
analyticalALlWins m = case cramersRule m {prize' = join bimap (10000000000000 +) $ prize' m} of
  Just presses -> [presses]
  _ -> []

determinant :: Machine -> Int
determinant (Machine (Button _ (x, y), Button _ (x', y')) _) = x * y' - y * x'

-- >>> map isLinearlyIndependent <$> loadInput "example.txt"
-- [True,True,True,True]
isLinearlyIndependent :: Machine -> Bool
isLinearlyIndependent = (/= 0) . determinant

-- "In linear algebra, Cramer's rule is an explicit formula for the solution of a system of linear equations
-- with as many equations as unknowns, valid whenever the system has a unique solution."
-- ~ https://en.wikipedia.org/wiki/Cramer%27s_rule
-- >>> map cramersRule <$> loadInput "example.txt"
-- [Just (80,40),Nothing,Just (38,86),Nothing]
cramersRule :: Machine -> Maybe (Int, Int)
cramersRule m@(Machine (Button _ (x, y), Button _ (x', y')) (tx, ty)) =
  let d = determinant m
      numeratorA = (tx * y' - ty * x')
      numeratorB = ((-tx) * y + ty * x)
   in case (numeratorA `mod` d, numeratorB `mod` d) of
        (0, 0) -> Just (numeratorA `div` d, numeratorB `div` d)
        _ -> Nothing

-- >>> part2 <$> loadInput "example.txt"
-- 875318608908
part2 :: [Machine] -> Int
part2 = costToWinAllPossible . map analyticalALlWins
