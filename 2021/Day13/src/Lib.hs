{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    followInstructions,
    renderPixels,
    pixelCount,
  )
where

import Control.Arrow (Arrow (first, second))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.List (intercalate)
import qualified Data.Set as S

type Coordinate = (Int, Int)

-- "(fold along x=, 5)"
type Instruction = (String, Int)

type Pixels = S.Set Coordinate

type Input = (Pixels, [Instruction])

-- >>> A.parseOnly parseCoordinate "6,10"
-- Right (6,10)
parseCoordinate :: A.Parser Coordinate
parseCoordinate = do
  x <- A.decimal
  ","
  y <- A.decimal
  return (x, y)

-- >>> A.parseOnly parseInstruction "fold along y=7"
-- Right ("fold along y=",7)
parseInstruction :: A.Parser Instruction
parseInstruction = do
  text <- A.choice $ map A.string ["fold along x=", "fold along y="]
  value <- A.decimal
  return (BSC.unpack text, value)

parseInput :: A.Parser Input
parseInput = do
  pixels <- S.fromList <$> A.sepBy1 parseCoordinate A.endOfLine
  A.count 2 A.endOfLine
  instructions <- A.sepBy1 parseInstruction A.endOfLine
  return (pixels, instructions)

-- >>> loadInput "example.txt"
-- (fromList [(0,3),(0,13),(0,14),(1,10),(2,14),(3,0),(3,4),(4,1),(4,11),(6,0),(6,10),(6,12),(8,4),(8,10),(9,0),(9,10),(10,4),(10,12)],[("fold along y=",7),("fold along x=",5)])
loadInput :: String -> IO Input
loadInput fileName =
  fromRight (S.empty, [])
    . A.parseOnly parseInput
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> foldX (S.fromList [(0, 0), (3, 1)]) 2
-- fromList [(0,0),(1,1)]
foldX :: Pixels -> Int -> Pixels
foldX pixels value = S.union left $ S.map (first ((2 * value) -)) right
  where
    (left, right) = S.partition (\(x, _) -> x < value) pixels

foldY :: Pixels -> Int -> Pixels
foldY pixels value = S.union top $ S.map (second ((2 * value) -)) bottom
  where
    (top, bottom) = S.partition (\(_, y) -> y < value) pixels

followInstruction :: Pixels -> Instruction -> Pixels
followInstruction pixels (instruction, value) = case instruction of
  "fold along x=" -> foldX pixels value
  "fold along y=" -> foldY pixels value
  _ -> pixels

-- >>> followInstructions <$> loadInput "example.txt"
-- fromList [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(2,4),(3,0),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)]
followInstructions :: Foldable t => (Pixels, t Instruction) -> Pixels
followInstructions (pixels, instructions) = foldl followInstruction pixels instructions

pixelCount :: Pixels -> Int
pixelCount = S.size

-- >>> renderPixels . followInstructions <$> loadInput "example.txt"
-- "#####\n#...#\n#...#\n#...#\n#####"
renderPixels :: Pixels -> String
renderPixels pixels = intercalate "\n" ([concat [if S.member (x, y) pixels then "#" else "." | x <- [0 .. xMax]] | y <- [0 .. yMax]])
  where
    xMax = maximum $ map fst $ S.toList pixels
    yMax = maximum $ map snd $ S.toList pixels
