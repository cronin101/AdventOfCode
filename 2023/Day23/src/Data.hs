{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Data where

import Control.Arrow (first)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

type Coord2D = (Int, Int)

data Slope = LeftSlope | RightSlope | UpSlope | DownSlope deriving (Eq, Ord)

type CompressedEdge = (Coord2D, Coord2D, Int)

instance Show Slope where
  show :: Slope -> String
  show LeftSlope = "<"
  show RightSlope = ">"
  show UpSlope = "^"
  show DownSlope = "v"

instance Read Slope where
  readsPrec :: Int -> String -> [(Slope, String)]
  readsPrec _ ('<' : xs) = [(LeftSlope, xs)]
  readsPrec _ ('>' : xs) = [(RightSlope, xs)]
  readsPrec _ ('^' : xs) = [(UpSlope, xs)]
  readsPrec _ ('v' : xs) = [(DownSlope, xs)]
  readsPrec _ _ = []

data Entity = Slope Slope | Forest | Path deriving (Eq, Ord)

type Point = (Coord2D, Entity)

type Map = M.Map Coord2D Entity

instance Show Entity where
  show :: Entity -> String
  show (Slope s) = show s
  show Forest = "#"
  show Path = "."

instance Read Entity where
  readsPrec :: Int -> String -> [(Entity, String)]
  readsPrec _ ('#' : xs) = [(Forest, xs)]
  readsPrec _ ('.' : xs) = [(Path, xs)]
  readsPrec i (c : xs) = first Slope <$> (readsPrec i (c : xs) :: [(Slope, String)])
  readsPrec _ _ = []

data MapSegment = MapSegment
  { entryPoint :: Coord2D,
    paths :: S.Set Coord2D,
    slopes :: M.Map Coord2D Slope,
    segment :: Map
  }
  deriving (Eq, Ord)

data PathState = PathState
  { start :: Coord2D,
    end :: Coord2D,
    current :: Coord2D,
    history :: S.Set Coord2D
  }
  deriving (Show, Eq, Ord)

data Hop = Hop {from :: Coord2D, to :: Coord2D, len :: Int} deriving (Show, Eq, Ord)

instance Show MapSegment where
  show :: MapSegment -> String
  show (MapSegment c ps ss m) =
    "MapSegment[entry="
      ++ show c
      ++ "](area="
      ++ show (length ps)
      ++ ")[exits="
      ++ show (M.toList ss)
      ++ "]"
      ++ "\n"
      ++ printMap m
      ++ "\n"

printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [" " ++ fromMaybe " " (M.lookup (x, y) m) | x <- [minX .. maxX]] | y <- reverse [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds m

printMap :: Map -> String
printMap = printGrid . M.map show

-- >>> bounds <$> loadInput "example.txt"
-- ((1,0),(21,22))
bounds :: M.Map Coord2D a -> (Coord2D, Coord2D)
bounds g
  | M.null g = ((0, 0), (0, 0))
  | otherwise = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (xs, ys) = unzip $ M.keys g
