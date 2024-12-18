{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.PSQueue (Binding ((:->)))
import Data.PSQueue qualified as PSQ
import Data.Set qualified as S

type Coord2D = (Int, Int)

data Input where
  Input ::
    {_bounds :: Coord2D, _bytes :: [Coord2D]} ->
    Input
  deriving (Show)

-- The coordinates of the corrupted bytes are parsed from the input file.
-- These bytes will fall into the memory space, corrupting it.
-- >>> A.parseOnly parseCoord "5,4"
-- Right (5,4)
parseCoord :: A.Parser Coord2D
parseCoord = (,) <$> A.decimal <* A.char ',' <*> A.decimal

-- Parses multiple coordinates of corrupted bytes from the input file.
parseCoords :: A.Parser [Coord2D]
parseCoords = parseCoord `A.sepBy` A.endOfLine

-- Simulates the bytes that have fallen into the memory space up to a given time.
-- The fallen bytes are represented as a set of coordinates.
-- >>> (`fallenAtTime` 12) <$> loadInput (6, 6) "example.txt"
-- fromList [(0,6),(1,5),(2,1),(2,4),(2,6),(3,0),(3,3),(4,2),(4,5),(5,1),(5,4),(6,3)]
fallenAtTime :: Input -> Int -> S.Set Coord2D
fallenAtTime s t = S.fromList $ take t $ _bytes s

-- Loads the input file and parses the coordinates of the corrupted bytes.
-- >>> loadInput (6, 6) "example.txt"
-- State {_bounds = (6,6), _bytes = [(5,4),(4,2),(4,5),(3,0),(2,1),(6,3),(2,4),(1,5),(0,6),(3,3),(2,6),(5,1),(1,2),(5,5),(2,5),(6,5),(1,4),(0,4),(6,4),(1,1),(6,1),(1,0),(0,5),(1,6),(2,0)]}
loadInput :: Coord2D -> [Char] -> IO Input
loadInput bounds = (Input bounds . fromRight [] . A.parseOnly parseCoords <$>) . BSC.readFile . ("src/" ++)

-- Implements Dijkstra's algorithm to find the shortest path from the top left corner to the exit.
-- The path must avoid corrupted memory coordinates and stay within the bounds of the memory space.
dijkstra :: Coord2D -> S.Set Coord2D -> M.Map Coord2D Int
dijkstra (maxX, maxY) corrupted = go M.empty $ PSQ.fromList [(0, 0) :-> 0]
  where
    inBounds (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
    go costs edges = case PSQ.minView edges of
      Nothing -> costs -- No more edges to explore
      Just (edge@(x, y) :-> cost, edges') -> case M.lookup edge costs of
        Just _ -> go costs edges' -- Already visited
        Nothing ->
          -- New edge
          let neighbours = filter (\c -> inBounds c && c `S.notMember` corrupted && c `M.notMember` costs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
              edges'' = foldl (\acc e -> PSQ.insert e (cost + 1) acc) edges' neighbours
              costs' = M.insert edge cost costs
           in go costs' edges''

-- Determines the minimum number of steps needed to reach the exit after the first t bytes have fallen.
-- >>> part1 12 <$> loadInput (6, 6) "example.txt"
-- Just 22
part1 :: Int -> Input -> Maybe Int
part1 t i@(Input bounds _) = bounds `M.lookup` dijkstra bounds (i `fallenAtTime` t)

-- Determines the coordinates of the first byte that will prevent the exit from being reachable.
-- >>> part2  <$> loadInput (6,6) "example.txt"
-- (6,1)
part2 :: Input -> Coord2D
part2 i = fst $ last $ takeWhile (isNothing . snd) $ map (\(t, b) -> (b, part1 t i)) $ reverse $ zip [1 ..] (_bytes i)
