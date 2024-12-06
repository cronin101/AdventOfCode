{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Arrow (first)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (find, intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)

type Coord2D = (Int, Int)

data Direction = N | E | S | W
  deriving (Show, Eq, Ord, Enum)

turnRight :: Direction -> Direction
turnRight W = N
turnRight d = succ d

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
stringMappings = M.fromList $ map swap (M.toList tileMappings)

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

bounds :: M.Map Coord2D a -> (Coord2D, Coord2D)
bounds g
  | M.null g = ((0, 0), (0, 0))
  | otherwise = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (xs, ys) = unzip $ M.keys g

printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [fromMaybe " " (M.lookup (x, y) m) | x <- [minX .. maxX]] | y <- reverse [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds m

instance Show Map where
  show (Map tiles guard path _) = printGrid . M.union (M.fromList (map ((,"X") . fst) $ S.toList path)) . M.map show $ M.union (maybe M.empty (uncurry M.singleton) guard) tiles

-- >>> A.parseOnly parseRow "....#....."
-- Right [((0,0),.),((1,0),.),((2,0),.),((3,0),.),((4,0),#),((5,0),.),((6,0),.),((7,0),.),((8,0),.),((9,0),.)]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = zipWith (\x -> ((x, 0),)) [0 ..] . map (read . BSC.unpack) <$> A.many1 (A.choice $ map A.string $ M.keys stringMappings)

parseMap :: A.Parser Map
parseMap = extractGuard . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    extractGuard m = Map (M.insert c Floor m) guard S.empty False
      where
        guard@(Just (c, _)) = find ((\case Guard _ -> True; _ -> False) . snd) $ M.toList m

-- >>> solve <$> loadInput "example.txt"
-- ....#.....
-- ....XXXXX#
-- ....X...X.
-- ..#.X...X.
-- ..XXXXX#X.
-- ..X.X.X.X.
-- .#XXXXXXX.
-- .XXXXXXX#.
-- #XXXXXXX..
-- ......#X..
solve :: Map -> Map
solve = head . dropWhile (\case Map _ Nothing _ _ -> False; Map _ _ _ True -> False; _ -> True) . iterate step

-- >>> step <$> loadInput "example.txt"
-- ....#.....
-- .........#
-- ..........
-- ..#.......
-- .......#..
-- ....^.....
-- .#..X.....
-- ........#.
-- #.........
-- ......#...
step :: Map -> Map
step map@(Map _ _ _ True) = map
step map@(Map _ Nothing _ _) = map
step (Map tiles (Just (c@(x, y), Guard direction)) path _) = Map tiles guard' path' (S.size path == S.size path')
  where
    guard' = case M.lookup c' tiles of
      Just Obstacle -> Just (c, Guard (turnRight direction))
      Just Floor -> Just (c', Guard direction)
      _ -> Nothing
    path' = S.insert (c, direction) path
    c' = case direction of
      N -> (x, y + 1)
      E -> (x + 1, y)
      S -> (x, y - 1)
      W -> (x - 1, y)

-- >>> loadInput "example.txt"
-- ....#.....
-- .........#
-- ..........
-- ..#.......
-- .......#..
-- ..........
-- .#..^.....
-- ........#.
-- #.........
-- ......#...
loadInput :: [Char] -> IO Map
loadInput = (fromRight (Map M.empty Nothing S.empty False) . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 41
part1 :: Map -> Int
part1 = S.size . S.map fst . path . solve

-- >>> part2 <$> loadInput "example.txt"
-- 6
part2 :: Map -> Int
part2 m =
  length $ filter isLooping $ map (\o -> solve $ m {tiles = M.insert o Obstacle $ tiles m}) $ S.toList potentialObstructions
  where
    Just (c, _) = guard m
    potentialObstructions = S.delete c $ S.map fst $ path $ solve m
