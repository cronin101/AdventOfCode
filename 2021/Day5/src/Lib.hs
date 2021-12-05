{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    pointsTraversedMoreThanOnce,
    isCardinalDirection,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import qualified Data.Map as M

type Coordinate = (Int, Int)

type Line = (Coordinate, Coordinate)

type Input = [Line]

-- >>> A.parseOnly parseCoordinate "0,9"
-- Right (0,9)
parseCoordinate :: A.Parser Coordinate
parseCoordinate = do
  x <- A.decimal
  A.string ","
  y <- A.decimal
  return (x, y)

-- >>> A.parseOnly parseLine "0,9 -> 5,9"
-- Right ((0,9),(5,9))
parseLine :: A.Parser Line
parseLine = do
  start <- parseCoordinate
  A.string " -> "
  end <- parseCoordinate
  return (start, end)

-- >>> A.parseOnly parseLines "8,0 -> 0,8\n9,4 -> 3,4"
-- Right [((8,0),(0,8)),((9,4),(3,4))]
parseLines :: A.Parser Input
parseLines = A.sepBy1 parseLine A.endOfLine

-- >>> isCardinalDirection ((0, 1), (0, 2))
-- True
-- >>> isCardinalDirection ((0, 1), (1, 2))
-- False
isCardinalDirection :: Line -> Bool
isCardinalDirection ((startX, startY), (endX, endY))
  | startX == endX = True
  | startY == endY = True
  | otherwise = False

-- >>> traversedPoints ((0,1), (0, 4))
-- [(0,1),(0,2),(0,3),(0,4)]
-- >>> traversedPoints ((0, 1), (4, 5))
-- [(0,1),(1,2),(2,3),(3,4),(4,5)]
traversedPoints :: Line -> [Coordinate]
traversedPoints ((startX, startY), (endX, endY)) = [(startX + (dX * step), startY + (dY * step)) | step <- [0 .. steps]]
  where
    steps = max (abs (endX - startX)) (abs (endY - startY))
    dX = if steps /= 0 then (endX - startX) `div` steps else 0
    dY = if steps /= 0 then (endY - startY) `div` steps else 0

-- >>> loadInput "example.txt"
-- [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
loadInput :: [Char] -> IO Input
loadInput fileName =
  fromRight []
    . A.parseOnly parseLines
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> countTraversedPoints [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
-- fromList [((0,0),1),((0,8),1),((0,9),2),((1,1),1),((1,7),1),((1,9),2),((2,2),2),((2,6),1),((2,9),2),((3,3),1),((3,4),1),((3,5),1),((3,9),1),((4,4),2),((4,9),1),((5,3),1),((5,5),2),((5,9),1),((6,2),1),((6,4),1),((6,6),1),((7,0),1),((7,1),2),((7,2),1),((7,3),2),((7,4),1),((7,7),1),((8,0),1),((8,2),1),((8,8),1),((9,4),1)]
countTraversedPoints :: Input -> M.Map Coordinate Integer
countTraversedPoints = M.unionsWith (+) . map (M.fromList . flip zip (repeat 1) . traversedPoints)

-- >>> pointsTraversedMoreThanOnce [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
-- [(0,9),(1,9),(2,2),(2,9),(4,4),(5,5),(7,1),(7,3)]
pointsTraversedMoreThanOnce :: Input -> [Coordinate]
pointsTraversedMoreThanOnce = map fst . filter ((> 1) . snd) . M.toList . countTraversedPoints

-- >>> filter isCardinalDirection $ [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
-- [((0,9),(5,9)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((0,9),(2,9)),((3,4),(1,4))]

-- >>> pointsTraversedMoreThanOnce $ filter isCardinalDirection $ [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
-- [(0,9),(1,9),(2,9),(3,4),(7,4)]
