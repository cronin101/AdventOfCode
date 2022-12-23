{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    showWorld,
    solve,
    sandCount,
    addFloor,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Ix (Ix (inRange, range))
import Data.List (intercalate)
import Data.Map qualified as M

type Point = (Int, Int)

type Structure = [Point]

data Entity = Wall | Sand | SandSource deriving (Eq)

-- ((xMin, xMax), (yMin, yMax), Active Sand, Entities)
type World = ((Point, Point), Maybe Point, M.Map Point Entity)

instance Show Entity where
  show Wall = "#"
  show Sand = "o"
  show SandSource = "+"

-- >>> A.parseOnly parsePoint "498,4"
-- Right (498,4)
parsePoint :: A.Parser Point
parsePoint = (,) <$> A.decimal <* A.char ',' <*> A.decimal

-- >>> A.parseOnly parseStructure "498,4 -> 498,6 -> 496,6"
-- Right [(498,4),(498,5),(498,6),(497,6),(496,6)]
parseStructure :: A.Parser Structure
parseStructure = fillInStructure <$> A.sepBy1 parsePoint " -> "

-- >>> A.parseOnly parseStructures "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
-- Right [[(498,4),(498,5),(498,6),(497,6),(496,6)],[(503,4),(502,4),(502,5),(502,6),(502,7),(502,8),(502,9),(501,9),(500,9),(499,9),(498,9),(497,9),(496,9),(495,9),(494,9)]]
parseStructures :: A.Parser [Structure]
parseStructures = A.sepBy1 parseStructure A.endOfLine

loadInput :: String -> IO World
loadInput = (withBounds . M.insert (500, 0) SandSource . M.fromList . concatMap (map (,Wall)) . fromRight [] . A.parseOnly parseStructures <$>) . BSC.readFile . ("src/" ++)
  where
    withBounds :: M.Map Point Entity -> World
    withBounds m = (((minimum xs, maximum xs), (minimum ys, maximum ys)), Nothing, m)
      where
        xs = map fst $ M.keys m
        ys = map snd $ M.keys m

-- >>> fillInStructure [(498,4),(498,6),(496,6)]
-- [(498,4),(498,5),(498,6),(497,6),(496,6)]
fillInStructure :: Structure -> Structure
fillInStructure ((x1, y1) : p@(x2, y2) : ps) = [(x, y) | x <- xs, y <- ys, (x, y) /= p] ++ fillInStructure (p : ps)
  where
    xs = if x2 > x1 then [x1 .. x2] else reverse [x2 .. x1]
    ys = if y2 > y1 then [y1 .. y2] else reverse [y2 .. y1]
fillInStructure rest = rest

showWorld :: World -> String
showWorld (((xMin, xMax), (yMin, yMax)), activeSand, m) =
  intercalate
    "\n"
    [ concat
        [ if Just (x, y) == activeSand then "●" else maybe " " show (M.lookup (x, y) m) | x <- [xMin .. xMax]
        ]
      | y <- [yMin .. yMax]
    ]

step :: World -> World
step (bounds, Nothing, m) = (bounds, Just (500, 0), m)
step (bounds, Just s@(x, y), m) = case M.lookup below m of
  Nothing -> (bounds, Just below, m)
  Just _ -> case M.lookup diagonalLeft m of
    Nothing -> (bounds, Just diagonalLeft, m)
    Just _ -> case M.lookup diagonalRight m of
      Nothing -> (bounds, Just diagonalRight, m)
      Just _ -> (bounds, Nothing, M.insert s Sand m)
  where
    below = (x, y + 1)
    diagonalLeft = (x - 1, y + 1)
    diagonalRight = (x + 1, y + 1)

addFloor :: World -> World
addFloor ((_, (yMin, yMax)), activeSand, m) = ((xRange, (yMin, yMax + 2)), activeSand, withFloor m)
  where
    yMax' = yMax + 2
    xRange = (500 - yMax', 500 + yMax')
    withFloor = M.union (M.fromList [((x, yMax'), Wall) | x <- range xRange])

solve :: World -> World
solve = head . dropWhile sandInPlay . iterate step
  where
    sandInPlay (_, Nothing, _) = True
    sandInPlay ((xRange, yRange), Just (x, y), m) = inRange xRange x && inRange yRange y && M.lookup (500, 0) m == Just SandSource

sandCount :: World -> Int
sandCount = M.size . M.filter (== Sand) . (\(_, _, m) -> m)
