{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))

data Direction = Up' | Down' | Left' | Right' deriving (Show, Eq, Ord)

data Instruction = Instruction {_dir :: Direction, _steps :: Int, _colour :: String} deriving (Show, Eq, Ord)

type Coord2D = (Int, Int)

parseDirection :: A.Parser Direction
parseDirection = "U" $> Up' <|> "D" $> Down' <|> "L" $> Left' <|> "R" $> Right'

-- >>> A.parseOnly parseInstruction "R 6 (#70c710)"
-- Right (Instruction {_dir = Right', _steps = 6, _colour = "70c710"})
parseInstruction :: A.Parser Instruction
parseInstruction = do
  (dir, steps) <- (,) <$> (parseDirection <* " ") <*> A.decimal <* " "
  colour <- BSC.unpack <$> ("(#" *> A.takeTill (== ')') <* ")")
  pure $ Instruction dir steps colour

parseInstructions :: A.Parser [Instruction]
parseInstructions = parseInstruction `A.sepBy1` A.endOfLine

-- >>> loadInput "example.txt"
-- [Instruction {_dir = Right', _steps = 6, _colour = "70c710"},Instruction {_dir = Down', _steps = 5, _colour = "0dc571"},Instruction {_dir = Left', _steps = 2, _colour = "5713f0"},Instruction {_dir = Down', _steps = 2, _colour = "d2c081"},Instruction {_dir = Right', _steps = 2, _colour = "59c680"},Instruction {_dir = Down', _steps = 2, _colour = "411b91"},Instruction {_dir = Left', _steps = 5, _colour = "8ceee2"},Instruction {_dir = Up', _steps = 2, _colour = "caa173"},Instruction {_dir = Left', _steps = 1, _colour = "1b58a2"},Instruction {_dir = Up', _steps = 2, _colour = "caa171"},Instruction {_dir = Right', _steps = 2, _colour = "7807d2"},Instruction {_dir = Up', _steps = 3, _colour = "a77fa3"},Instruction {_dir = Left', _steps = 2, _colour = "015232"},Instruction {_dir = Up', _steps = 2, _colour = "7a21e3"}]
loadInput :: [Char] -> IO [Instruction]
loadInput = (fromRight [] . A.parseOnly parseInstructions <$>) . BSC.readFile . ("src/" ++)

move :: Direction -> Int -> Coord2D -> Coord2D
move Up' n (x, y) = (x, y - n)
move Down' n (x, y) = (x, y + n)
move Left' n (x, y) = (x - n, y)
move Right' n (x, y) = (x + n, y)

-- >>> followInstructions <$> loadInput "example.txt"
-- [(6,0),(6,5),(4,5),(4,7),(6,7),(6,9),(1,9),(1,7),(0,7),(0,5),(2,5),(2,2),(0,2),(0,0)]
followInstructions :: [Instruction] -> [Coord2D]
followInstructions = drop 1 . reverse . foldl followInstruction [(0, 0)]
  where
    followInstruction :: [Coord2D] -> Instruction -> [Coord2D]
    followInstruction ps@(position : _) (Instruction _dir _steps _) = let position' = move _dir _steps position in (position' : ps)

-- https://en.wikipedia.org/wiki/Shoelace_formula#Trapezoid_formula
-- >>> shoelaceTrapezoid . followInstructions <$> loadInput "example.txt"
-- 42
shoelaceTrapezoid :: [Coord2D] -> Int
shoelaceTrapezoid steps =
  let f (x, y) (x', y') = (x' - x) * (y' + y)
   in abs (sum (zipWith f (drop 1 steps ++ [head steps]) steps)) `div` 2

-- !!!! This took a lot of Wikipedia spelunking and a little peeking at Reddit
-- A = i + b / 2 - 1
-- Shoelace theorem gives us A but we want i + perimeter
-- i = A - b / 2 + 1
-- adding perimeter brings it to A + b / 2 + 1
-- >>> areaIncludingPerimeter  <$> loadInput "example.txt"
-- 62
areaIncludingPerimeter :: [Instruction] -> Int
areaIncludingPerimeter is =
  let b = sum $ map _steps is
      a' = shoelaceTrapezoid $ followInstructions is
   in a' + (b `div` 2) + 1

-- >>> part1 <$> loadInput "example.txt"
-- 62
part1 :: [Instruction] -> Int
part1 = areaIncludingPerimeter

-- >>> map fixInstruction <$> loadInput "example.txt"
-- [Instruction {_dir = Right', _steps = 461937, _colour = "70c710"},Instruction {_dir = Down', _steps = 56407, _colour = "0dc571"},Instruction {_dir = Right', _steps = 356671, _colour = "5713f0"},Instruction {_dir = Down', _steps = 863240, _colour = "d2c081"},Instruction {_dir = Right', _steps = 367720, _colour = "59c680"},Instruction {_dir = Down', _steps = 266681, _colour = "411b91"},Instruction {_dir = Left', _steps = 577262, _colour = "8ceee2"},Instruction {_dir = Up', _steps = 829975, _colour = "caa173"},Instruction {_dir = Left', _steps = 112010, _colour = "1b58a2"},Instruction {_dir = Down', _steps = 829975, _colour = "caa171"},Instruction {_dir = Left', _steps = 491645, _colour = "7807d2"},Instruction {_dir = Up', _steps = 686074, _colour = "a77fa3"},Instruction {_dir = Left', _steps = 5411, _colour = "015232"},Instruction {_dir = Up', _steps = 500254, _colour = "7a21e3"}]
fixInstruction :: Instruction -> Instruction
fixInstruction (Instruction _dir _steps _colour) = Instruction dir' steps' _colour
  where
    dir' = case last _colour of
      '0' -> Right'
      '1' -> Down'
      '2' -> Left'
      '3' -> Up'
      _ -> error "Invalid direction"
    steps' = read ("0x" ++ take 5 _colour)

-- >>> part2 <$> loadInput "example.txt"
-- 952408144115
part2 :: [Instruction] -> Int
part2 = part1 . map fixInstruction
