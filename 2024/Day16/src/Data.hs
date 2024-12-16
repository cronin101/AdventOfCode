{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Data where

import Data.Map qualified as M
import Data.Set qualified as S

type Coord2D = (Int, Int)

type Map = M.Map Coord2D Tile

data Tile = Wall | Start | End deriving (Eq)

data Direction = North | East | South | West deriving (Eq, Ord, Enum)

instance Show Direction where
  show :: Direction -> String
  show North = "^"
  show East = ">"
  show South = "v"
  show West = "<"

instance Show Tile where
  show :: Tile -> String
  show Wall = "#"
  show Start = "S"
  show End = "E"

type Position = (Coord2D, Direction)

data World = World
  { wMap :: Map,
    wStart :: Position,
    wEnd :: Coord2D,
    wCosts :: M.Map Position Int,
    wPrevious :: M.Map Position (S.Set Position)
  }

toVector :: Direction -> Coord2D
toVector North = (0, -1)
toVector South = (0, 1)
toVector East = (1, 0)
toVector West = (-1, 0)

toReversed :: Direction -> Direction
toReversed North = South
toReversed South = North
toReversed East = West
toReversed West = East

clockWise :: Direction -> Direction
clockWise West = North
clockWise d = succ d

counterClockWise :: Direction -> Direction
counterClockWise North = West
counterClockWise d = pred d

add :: Coord2D -> Coord2D -> Coord2D
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
