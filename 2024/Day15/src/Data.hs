{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Data where

import Data.List (intercalate)
import Data.Map qualified as M

data Tile = Wall | Box | Robot | LeftBox | RightBox deriving (Eq, Ord)

instance Show Tile where
  show :: Tile -> String
  show Wall = "#"
  show Box = "O"
  show Robot = "@"
  show LeftBox = "["
  show RightBox = "]"

data Direction = U | D | L | R deriving (Eq)

instance Show Direction where
  show :: Direction -> String
  show U = "^"
  show D = "v"
  show L = "<"
  show R = ">"

type Coord2D = (Int, Int)

data Room where
  Room :: {tiles' :: M.Map Coord2D Tile} -> Room

instance Show Room where
  show :: Room -> String
  show a = "\n" ++ intercalate "\n" ([concat [maybe "." show (M.lookup (x, y) (tiles' a)) | x <- [minX .. maxX]] | y <- reverse [minY .. maxY]])
    where
      coords = M.keys . tiles' $ a
      minX = minimum . map fst $ coords
      maxX = maximum . map fst $ coords
      minY = minimum . map snd $ coords
      maxY = maximum . map snd $ coords

data State = State
  { room' :: Room,
    robot' :: Coord2D,
    directions' :: [Direction]
  }
  deriving (Show)
