{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data (stringMappings, Coord2D, Direction (N, S, E, W), Map (Map, tiles, guard, path, isLooping), Tile (Floor, Obstacle, Guard)) where

import Data.ByteString.Char8 qualified as BSC
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)

type Coord2D = (Int, Int)

data Direction = N | E | S | W
  deriving (Show, Eq, Ord, Enum)

data Tile = Floor | Obstacle | Guard Direction deriving (Eq, Ord)

tileMappings :: M.Map Tile BSC.ByteString
tileMappings =
  M.fromList
    [ (Floor, "."),
      (Obstacle, "#"),
      (Guard N, "^"),
      (Guard E, ">"),
      (Guard S, "v"),
      (Guard W, "<")
    ]

instance Show Tile where
  show tile = case M.lookup tile tileMappings of
    Just str -> BSC.unpack str
    Nothing -> error "Unknown tile"

stringMappings :: M.Map BSC.ByteString Tile
stringMappings = M.fromList $ map swap $ M.toList tileMappings

instance Read Tile where
  readsPrec _ str = case M.lookup (BSC.pack str) stringMappings of
    Just tile -> [(tile, "")]
    Nothing -> []

data Map = Map
  { tiles :: M.Map Coord2D Tile,
    guard :: Maybe (Coord2D, Tile),
    path :: S.Set (Coord2D, Direction),
    isLooping :: Bool
  }

instance Show Map where
  show (Map tiles guard path _) = printGrid . addPath . M.map show $ addGuard tiles
    where
      addGuard = M.union (maybe M.empty (uncurry M.singleton) guard)
      addPath = M.union (M.fromList (map ((,"X") . fst) $ S.toList path))

-- The bounds of the map, defining the edges of the known world.
bounds :: M.Map Coord2D a -> (Coord2D, Coord2D)
bounds g
  | M.null g = ((0, 0), (0, 0))
  | otherwise = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (xs, ys) = unzip $ M.keys g

-- Print the grid, revealing the layout of the lab and the guard's patrol path.
printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [fromMaybe " " (M.lookup (x, y) m) | x <- [minX .. maxX]] | y <- reverse [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds m