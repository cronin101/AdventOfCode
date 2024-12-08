{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data (bounds, Bounds, Coord2D, Space (Space, tiles, antiNodes), Tile (Empty, Antenna, frequency), Frequency, Antinode (Initial, Resonant)) where

import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

type Coord2D = (Int, Int)

type Frequency = Char

data Tile = Antenna {frequency :: Frequency} | Empty

type Bounds = (Coord2D, Coord2D)

data Antinode = Initial Coord2D | Resonant Coord2D
  deriving (Eq, Ord)

antinodeLocation :: Antinode -> Coord2D
antinodeLocation (Initial c) = c
antinodeLocation (Resonant c) = c

instance Show Tile where
  show Empty = "."
  show (Antenna f) = pure f

instance Read Tile where
  readsPrec _ "." = [(Empty, "")]
  readsPrec _ [c] = [(Antenna c, "")]
  readsPrec _ _ = []

data Space = Space
  { tiles :: M.Map Coord2D Tile,
    antiNodes :: S.Set Antinode
  }

instance Show Space where
  show (Space tiles antiNodes) = printGrid . addAntiNodes . M.map show $ tiles
    where
      antennae = M.filter (\case Antenna _ -> True; _ -> False) tiles
      addAntiNodes = M.union (M.fromList (map (\case (Initial c) -> (c, "#"); (Resonant c) -> (c, "▪")) $ S.toList $ S.filter (not . (`M.member` antennae) . antinodeLocation) antiNodes))

-- The bounds of the map, defining the edges of the known world.
bounds :: M.Map Coord2D a -> Bounds
bounds g
  | M.null g = ((0, 0), (0, 0))
  | otherwise = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (xs, ys) = unzip $ M.keys g

-- Print the grid, revealing the layout of the antennae and antinodes.
printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [fromMaybe " " (M.lookup (x, y) m) | x <- [minX .. maxX]] | y <- reverse [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds m