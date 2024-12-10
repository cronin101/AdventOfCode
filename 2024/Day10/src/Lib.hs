{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Set qualified as S

type Coord2D = (Int, Int)

type Grid = M.Map Coord2D Int

data World = World
  { trails :: S.Set Trail,
    grid :: Grid
  }
  deriving (Show)

data Trail where
  Trail ::
    { start :: Coord2D,
      visited :: S.Set Coord2D,
      end :: Coord2D,
      options :: S.Set Coord2D,
      height :: Int
    } ->
    Trail
  deriving (Show, Eq, Ord)

-- Parse a row of the topographic map into coordinates and heights
-- >>> A.parseOnly parseRow "0123"
-- Right [((0,0),0),((1,0),1),((2,0),2),((3,0),3)]
parseRow :: A.Parser [(Coord2D, Int)]
parseRow = zipWith (\x -> ((x, 0),)) [0 ..] . map (read . pure) <$> A.many1 A.digit

-- Parse the entire topographic map into a World structure
parseMap :: A.Parser World
parseMap = toWorld . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    toWorld g = World (trailHeads 0 g) g

-- Determine if two coordinates are neighbors (adjacent horizontally or vertically)
isNeighbor :: Coord2D -> Coord2D -> Bool
isNeighbor (x, y) n = n `elem` [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Identify all trailheads (starting points) at a given height
trailHeads :: Int -> Grid -> S.Set Trail
trailHeads height g =
  S.fromList
    $ map
      ( \(c, h) ->
          Trail
            { start = c,
              visited = S.singleton c,
              end = c,
              height = h,
              options = S.filter (isNeighbor c) $ S.map start $ trailHeads (h + 1) g
            }
      )
    $ filter ((== height) . snd)
    $ M.toList g

-- Perform a single step in the simulation, advancing all trails
step :: World -> World
step w
  | all (S.null . options) $ trails w = w
  | otherwise = w {trails = S.unions $ S.map (stepTrail $ grid w) $ trails w}

-- Advance a single trail by one step
stepTrail :: Grid -> Trail -> S.Set Trail
stepTrail g t
  | null $ options t = S.singleton t
  | otherwise =
      S.map
        ( \o ->
            t
              { visited = S.insert o $ visited t,
                end = o,
                height = height t + 1,
                options = S.filter (isNeighbor o) (S.map start $ trailHeads (height t + 2) g)
              }
        )
        $ options t

-- Solve the puzzle by repeatedly stepping until no more options are available
solve :: World -> World
solve = until (all (S.null . options) . trails) step

-- Load the input file and parse it into a World structure
-- >>> loadInput "example.txt"
-- World {trails = fromList [Trail {start = (0,1), visited = fromList [(0,1)], end = (0,1), options = fromList [(0,0),(1,1)], height = 0},Trail {start = (1,0), visited = fromList [(1,0)], end = (1,0), options = fromList [(0,0),(1,1)], height = 0},Trail {start = (2,2), visited = fromList [(2,2)], end = (2,2), options = fromList [(3,2)], height = 0},Trail {start = (2,7), visited = fromList [(2,7)], end = (2,7), options = fromList [(2,6),(3,7)], height = 0},Trail {start = (4,5), visited = fromList [(4,5)], end = (4,5), options = fromList [(4,6)], height = 0},Trail {start = (4,7), visited = fromList [(4,7)], end = (4,7), options = fromList [(3,7),(4,6),(5,7)], height = 0},Trail {start = (5,2), visited = fromList [(5,2)], end = (5,2), options = fromList [(6,2)], height = 0},Trail {start = (6,1), visited = fromList [(6,1)], end = (6,1), options = fromList [(6,2),(7,1)], height = 0},Trail {start = (6,3), visited = fromList [(6,3)], end = (6,3), options = fromList [(6,2)], height = 0}], grid = fromList [((0,0),1),((0,1),0),((0,2),3),((0,3),4),((0,4),9),((0,5),8),((0,6),7),((0,7),8),((1,0),0),((1,1),1),((1,2),2),((1,3),5),((1,4),6),((1,5),7),((1,6),8),((1,7),9),((2,0),4),((2,1),3),((2,2),0),((2,3),6),((2,4),5),((2,5),4),((2,6),1),((2,7),0),((3,0),5),((3,1),2),((3,2),1),((3,3),7),((3,4),4),((3,5),3),((3,6),2),((3,7),1),((4,0),6),((4,1),9),((4,2),9),((4,3),8),((4,4),9),((4,5),0),((4,6),1),((4,7),0),((5,0),7),((5,1),8),((5,2),0),((5,3),9),((5,4),8),((5,5),9),((5,6),8),((5,7),1),((6,0),3),((6,1),0),((6,2),1),((6,3),0),((6,4),7),((6,5),6),((6,6),7),((6,7),2),((7,0),2),((7,1),1),((7,2),2),((7,3),3),((7,4),4),((7,5),5),((7,6),4),((7,7),3)]}
loadInput :: [Char] -> IO World
loadInput = (fromRight (World S.empty M.empty) . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)

-- Calculate the sum of the scores of all trailheads
-- >>> part1 <$> loadInput "example.txt"
-- 36
part1 :: World -> Int
part1 = S.size . S.map (liftM2 (,) start end) . S.filter ((== 9) . height) . trails . solve

-- Calculate the sum of the ratings of all trailheads
-- >>> part2 <$> loadInput "example.txt"
-- 81
part2 :: World -> Int
part2 = S.size . S.filter ((9 ==) . height) . trails . solve
