{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (second), first)
import Data
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (intercalate, minimumBy)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.PSQueue (Binding ((:->)))
import Data.PSQueue qualified as PSQ
import Data.Set qualified as S

-- The World instance for Show, allowing us to visualize the maze.
instance Show World where
  show :: World -> String
  show = printGrid

-- Function to print the grid of the maze, showing the current state of the world.
printGrid :: World -> String
printGrid w = intercalate "\n" ([concat [fromMaybe "." $ M.lookup (x, y) (M.union (M.fromSet (const "O") $ bestTiles w) (M.map show (wMap w))) | x <- [minX .. maxX]] | y <- [minY .. maxY]])
  where
    coords = M.keys $ wMap w
    minX = minimum $ map fst coords
    maxX = maximum $ map fst coords
    minY = minimum $ map snd coords
    maxY = maximum $ map snd coords

-- Parser for individual tiles in the maze.
parseTile :: A.Parser Tile
parseTile = (Wall <$ "#") <|> (Start <$ "S") <|> (End <$ "E")

-- Parser for a row of tiles in the maze.
-- >>> A.parseOnly parseRow "#.......#....E#"
-- Right [((0,0),#),((8,0),#),((13,0),E),((14,0),#)]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = map (second fromJust) . filter (isJust . snd) . zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (Just <$> parseTile <|> Nothing <$ ".")

-- Parser for the entire world (maze).
parseWorld :: A.Parser World
parseWorld = toWorld . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    toWorld m =
      let start = (fst $ M.findMin $ M.filter (== Start) m, East)
          end = fst $ M.findMin $ M.filter (== End) m
       in World m start end M.empty M.empty

-- The main function to solve the maze using Dijkstra's algorithm.
-- >>> dijkstra <$> loadInput "example.txt"
-- ###############
-- #.......#....O#
-- #.#.###.#.###O#
-- #.....#.#...#O#
-- #.###.#####.#O#
-- #.#.#.......#O#
-- #.#.#####.###O#
-- #..OOOOOOOOO#O#
-- ###O#O#####O#O#
-- #OOO#O....#O#O#
-- #O#O#O###.#O#O#
-- #OOOOO#...#O#O#
-- #O###.#.#.#O#O#
-- #O..#.....#OOO#
-- ###############
dijkstra :: World -> World
dijkstra w = w {wCosts = wCosts', wPrevious = wPrevious'}
  where
    -- Initialize the Priority Search Queue with the starting position
    (wCosts', wPrevious') = go (wCosts w, M.singleton (wStart w) S.empty) $ PSQ.fromList [(Nothing, wStart w) :-> 0]
    -- Function to add the previous position to the set of previous positions for a given position
    withLastEdge previous (lastPosition, position) = case lastPosition of
      Nothing -> previous
      Just f -> M.insertWith S.union position (S.singleton f) previous
    go acc@(costs, previous) edges = case PSQ.minView edges of -- Continue until we have no more edges to explore
      Nothing -> acc -- We are done
      -- Get the next edge to explore
      Just (edge@(_, position@(_, direction)) :-> cost, edges') ->
        case position `M.lookup` costs of
          Just cost'
            | cost == cost' -> go (costs, previous `withLastEdge` edge) edges' -- This is one of the shortest paths
            | otherwise -> go acc edges' -- This is not the shortest path
          Nothing ->
            -- We have not visited this position before, so we must be on a shortest path
            let rotations =
                  [ (second clockWise position, cost + 1000),
                    (second counterClockWise position, cost + 1000)
                  ]
                stepPosition = first (add (toVector direction)) position
                -- If we hit a wall, we can only rotate
                neighbours = case M.lookup (fst stepPosition) (wMap w) of
                  Just Wall -> rotations
                  _ -> (stepPosition, cost + 1) : rotations
                -- Insert the new edges and costs into the Priority Search Queue
                edges'' = foldl (\e (c, newCost) -> PSQ.insert (Just position, c) newCost e) edges' neighbours
                -- Update the costs and previous positions
                costs' = M.insert position cost costs
             in go (costs', previous `withLastEdge` edge) edges''

-- Function to load the input file and parse it into the World structure.
-- >>>   loadInput "example.txt"
-- ###############
-- #.......#....E#
-- #.#.###.#.###.#
-- #.....#.#...#.#
-- #.###.#####.#.#
-- #.#.#.......#.#
-- #.#.#####.###.#
-- #...........#.#
-- ###.#.#####.#.#
-- #...#.....#.#.#
-- #.#.#.###.#.#.#
-- #.....#...#.#.#
-- #.###.#.#.#.#.#
-- #S..#.....#...#
-- ###############
loadInput :: [Char] -> IO World
loadInput = (fromRight (World M.empty ((0, 0), East) (0, 0) M.empty M.empty) . A.parseOnly parseWorld <$>) . BSC.readFile . ("src/" ++)

-- Function to calculate the minimum costs to reach each tile in the maze.
minCosts :: World -> M.Map Coord2D (Direction, Int)
minCosts = M.fromListWith (\a b -> if snd a <= snd b then a else b) . map (\((p, d), c) -> (p, (d, c))) . M.toList . wCosts

-- Function to calculate the score for part 1 of the puzzle.
-- >>> part1 <$> loadInput "example.txt"
-- 7036
part1 :: World -> Int
part1 w = snd $ (M.! wEnd w) $ minCosts $ dijkstra w

-- Function to step backwards through the maze to find the best path.
stepBackwards :: World -> S.Set Position -> S.Set Position
stepBackwards w s
  | S.null s = s
  | otherwise = S.unions $ S.fromList $ mapMaybe (\p -> M.lookup p (wPrevious w)) $ S.toList s

-- Function to find the best tiles to sit on in the maze.
bestTiles :: World -> S.Set Coord2D
bestTiles w = S.map fst $ S.unions $ takeWhile (not . S.null) $ iterate (stepBackwards w) bestEnd
  where
    bestEnd = case endings of
      [] -> S.empty
      es -> S.singleton $ fst $ minimumBy (compare `on` snd) es
    endings = filter (\((c, _), _) -> c == wEnd w) $ M.toList $ wCosts w

-- Function to calculate the score for part 2 of the puzzle.
-- >>> part2 <$> loadInput "example.txt"
-- 45
part2 :: World -> Int
part2 = S.size . bestTiles . dijkstra