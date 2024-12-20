{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (first), second)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.PSQueue (Binding ((:->)))
import Data.PSQueue qualified as PSQ
import Data.Set qualified as S

type Coord2D = (Int, Int)

-- (Start, Wall, End)
type WallClip = (Coord2D, Coord2D, Coord2D)

-- (Start, End)
type Cheat = (Coord2D, Coord2D)

data Tile = Wall | Start | End deriving (Eq)

data RaceTrack = RaceTrack
  { bounds' :: Coord2D,
    tiles' :: M.Map Coord2D Tile,
    end' :: Coord2D,
    distancesFromStart' :: M.Map Coord2D Int,
    distancesFromEnd' :: M.Map Coord2D Int
  }
  deriving (Eq)

instance Show RaceTrack where
  show :: RaceTrack -> String
  show = printRaceTrack

instance Show Tile where
  show :: Tile -> String
  show Wall = "#"
  show Start = "S"
  show End = "E"

printRaceTrack :: RaceTrack -> String
printRaceTrack (RaceTrack (maxX, maxY) m _ distancesFromStart distancesFromEnd) =
  let longestDistanceFromStartStringLength = maximum $ map (length . show) $ M.elems distancesFromStart
      longestDistanceFromEndStringLength = maximum $ map (length . show) $ M.elems distancesFromEnd
      longestDistanceStringLength = max longestDistanceFromStartStringLength longestDistanceFromEndStringLength
      pad str = let padding = replicate (longestDistanceStringLength - length str) ' ' in padding ++ str ++ " "
      minDistanceToExtreme = M.unionWith min distancesFromStart distancesFromEnd
      showTile (x, y) = pad $ fromMaybe "." $ M.lookup (x, y) $ M.unions [M.map show m, M.map show minDistanceToExtreme]
   in intercalate "\n" ([concat [showTile (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]])

parseTile :: A.Parser Tile
parseTile = (Wall <$ "#") <|> (Start <$ "S") <|> (End <$ "E")

-- >>> A.parseOnly parseRow "#S#...#.#.#...#"
-- Right [((0,0),#),((1,0),S),((2,0),#),((6,0),#),((8,0),#),((10,0),#),((14,0),#)]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = map (second fromJust) . filter (isJust . snd) . zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (Just <$> parseTile <|> Nothing <$ ".")

parseRaceTrack :: A.Parser RaceTrack
parseRaceTrack = toMap . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    toMap m =
      let start = S.findMin $ M.keysSet $ M.filter (== Start) m
          end = S.findMin $ M.keysSet $ M.filter (== End) m
          walls = M.keysSet $ M.filter (== Wall) m
          bounds = S.findMax $ M.keysSet m
          distancesFromStart = dijkstra bounds walls start
          distancesFromEnd = dijkstra bounds walls end
       in RaceTrack bounds m end distancesFromStart distancesFromEnd

-- >>> loadInput "example.txt"
--  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
--  #  2  3  4  # 10 11 12  # 26 27 28 29 30  #
--  #  1  #  5  #  9  # 13  # 25  #  #  # 31  #
--  #  S  #  6  7  8  # 14  # 24  # 34 33 32  #
--  #  #  #  #  #  #  # 15  # 23  # 35  #  #  #
--  #  #  #  #  #  #  # 16  # 22  # 36 37 38  #
--  #  #  #  #  #  #  # 17  # 21  #  #  # 39  #
--  #  #  #  2  1  E  # 18 19 20  # 42 41 40  #
--  #  #  #  3  #  #  #  #  #  #  # 41  #  #  #
--  #  6  5  4  #  #  # 24 25 26  # 40 39 38  #
--  #  7  #  #  #  #  # 23  # 27  #  #  # 37  #
--  #  8  # 14 15 16  # 22  # 28  # 34 35 36  #
--  #  9  # 13  # 17  # 21  # 29  # 33  #  #  #
--  # 10 11 12  # 18 19 20  # 30 31 32  #  #  #
--  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
loadInput :: [Char] -> IO RaceTrack
loadInput = (fromRight (RaceTrack (0, 0) M.empty (0, 0) M.empty M.empty) . A.parseOnly parseRaceTrack <$>) . BSC.readFile . ("src/" ++)

neighbours :: Coord2D -> [Coord2D]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- >>> glitchlessRecord <$> loadInput "example.txt"
-- 84
glitchlessRecord :: RaceTrack -> Int
glitchlessRecord m = distancesFromStart' m M.! end' m

dijkstra :: Coord2D -> S.Set Coord2D -> Coord2D -> M.Map Coord2D Int
dijkstra (maxX, maxY) walls start = go M.empty $ PSQ.fromList [start :-> 0]
  where
    inBounds (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
    go costs edges = case PSQ.minView edges of
      Nothing -> costs -- No more edges to explore
      Just (edge :-> cost, edges') -> case M.lookup edge costs of
        Just _ -> go costs edges' -- Already visited
        Nothing ->
          -- New edge
          let validNeighbours = filter (\c -> inBounds c && c `S.notMember` walls) $ neighbours edge
              edges'' = foldl (\acc e -> PSQ.insert e (cost + 1) acc) edges' validNeighbours
              costs' = M.insert edge cost costs
           in go costs' edges''

wallClipsForPoint :: RaceTrack -> Coord2D -> [WallClip]
wallClipsForPoint m p = [(p, w, e) | p `S.notMember` walls, w <- neighbours p, w `S.member` walls, e <- neighbours w, e `M.member` distancesFromEnd' m, e /= p]
  where
    walls = M.keysSet $ M.filter (== Wall) $ tiles' m

-- >>> allWallClips <$> loadInput "example.txt"
-- [((1,2),(2,2),(3,2)),((1,2),(2,2),(2,1)),((1,3),(2,3),(3,3)),((1,10),(2,10),(2,9)),((1,11),(2,11),(3,11)),((1,12),(2,12),(3,12)),((1,12),(2,12),(2,13)),((2,1),(2,2),(3,2)),((2,1),(2,2),(1,2)),((2,9),(2,10),(1,10)),((2,9),(2,8),(3,8)),((2,13),(2,12),(3,12)),((2,13),(2,12),(1,12)),((3,1),(4,1),(5,1)),((3,2),(4,2),(5,2)),((3,2),(4,2),(4,3)),((3,2),(2,2),(1,2)),((3,2),(2,2),(2,1)),((3,3),(2,3),(1,3)),((3,8),(4,8),(4,7)),((3,8),(2,8),(2,9)),((3,9),(3,10),(3,11)),((3,11),(2,11),(1,11)),((3,11),(3,10),(3,9)),((3,12),(4,12),(5,12)),((3,12),(4,12),(4,11)),((3,12),(2,12),(1,12)),((3,12),(2,12),(2,13)),((3,13),(4,13),(5,13)),((4,3),(4,2),(5,2)),((4,3),(4,2),(3,2)),((4,7),(4,8),(3,8)),((4,11),(4,12),(5,12)),((4,11),(4,12),(3,12)),((5,1),(4,1),(3,1)),((5,2),(6,2),(7,2)),((5,2),(6,2),(6,1)),((5,2),(4,2),(3,2)),((5,2),(4,2),(4,3)),((5,3),(6,3),(7,3)),((5,7),(6,7),(7,7)),((5,11),(6,11),(7,11)),((5,12),(6,12),(7,12)),((5,12),(6,12),(6,13)),((5,12),(4,12),(3,12)),((5,12),(4,12),(4,11)),((5,13),(4,13),(3,13)),((6,1),(6,2),(7,2)),((6,1),(6,2),(5,2)),((6,13),(6,12),(7,12)),((6,13),(6,12),(5,12)),((7,1),(8,1),(9,1)),((7,2),(8,2),(9,2)),((7,2),(6,2),(5,2)),((7,2),(6,2),(6,1)),((7,3),(8,3),(9,3)),((7,3),(6,3),(5,3)),((7,4),(8,4),(9,4)),((7,5),(8,5),(9,5)),((7,6),(8,6),(9,6)),((7,6),(8,6),(8,7)),((7,7),(6,7),(5,7)),((7,7),(7,8),(7,9)),((7,9),(7,8),(7,7)),((7,10),(8,10),(9,10)),((7,10),(8,10),(8,9)),((7,11),(8,11),(9,11)),((7,11),(6,11),(5,11)),((7,12),(8,12),(9,12)),((7,12),(6,12),(5,12)),((7,12),(6,12),(6,13)),((7,13),(8,13),(9,13)),((8,7),(8,8),(8,9)),((8,7),(8,6),(9,6)),((8,7),(8,6),(7,6)),((8,9),(8,10),(9,10)),((8,9),(8,10),(7,10)),((8,9),(8,8),(8,7)),((9,1),(8,1),(7,1)),((9,2),(10,2),(10,1)),((9,2),(8,2),(7,2)),((9,3),(10,3),(11,3)),((9,3),(8,3),(7,3)),((9,4),(10,4),(11,4)),((9,4),(8,4),(7,4)),((9,5),(10,5),(11,5)),((9,5),(8,5),(7,5)),((9,6),(8,6),(7,6)),((9,6),(8,6),(8,7)),((9,7),(10,7),(11,7)),((9,7),(9,8),(9,9)),((9,9),(10,9),(11,9)),((9,9),(9,8),(9,7)),((9,10),(8,10),(7,10)),((9,10),(8,10),(8,9)),((9,11),(10,11),(11,11)),((9,11),(8,11),(7,11)),((9,12),(10,12),(11,12)),((9,12),(10,12),(10,13)),((9,12),(8,12),(7,12)),((9,13),(8,13),(7,13)),((10,1),(10,2),(9,2)),((10,13),(10,12),(11,12)),((10,13),(10,12),(9,12)),((11,1),(11,2),(11,3)),((11,3),(10,3),(9,3)),((11,3),(11,2),(11,1)),((11,4),(12,4),(12,5)),((11,4),(12,4),(12,3)),((11,4),(10,4),(9,4)),((11,5),(10,5),(9,5)),((11,5),(11,6),(11,7)),((11,7),(10,7),(9,7)),((11,7),(11,6),(11,5)),((11,8),(12,8),(12,9)),((11,8),(12,8),(12,7)),((11,9),(10,9),(9,9)),((11,9),(11,10),(11,11)),((11,11),(10,11),(9,11)),((11,11),(11,10),(11,9)),((11,12),(12,12),(12,11)),((11,12),(10,12),(9,12)),((11,12),(10,12),(10,13)),((12,1),(12,2),(13,2)),((12,1),(12,2),(12,3)),((12,3),(12,4),(11,4)),((12,3),(12,4),(12,5)),((12,3),(12,2),(13,2)),((12,3),(12,2),(12,1)),((12,5),(12,6),(13,6)),((12,5),(12,6),(12,7)),((12,5),(12,4),(11,4)),((12,5),(12,4),(12,3)),((12,7),(12,8),(11,8)),((12,7),(12,8),(12,9)),((12,7),(12,6),(13,6)),((12,7),(12,6),(12,5)),((12,9),(12,10),(13,10)),((12,9),(12,10),(12,11)),((12,9),(12,8),(11,8)),((12,9),(12,8),(12,7)),((12,11),(12,12),(11,12)),((12,11),(12,10),(13,10)),((12,11),(12,10),(12,9)),((13,2),(12,2),(12,3)),((13,2),(12,2),(12,1)),((13,3),(13,4),(13,5)),((13,5),(13,4),(13,3)),((13,6),(12,6),(12,7)),((13,6),(12,6),(12,5)),((13,7),(13,8),(13,9)),((13,9),(13,8),(13,7)),((13,10),(12,10),(12,11)),((13,10),(12,10),(12,9))]
allWallClips :: RaceTrack -> [WallClip]
allWallClips m = concatMap (wallClipsForPoint m) $ M.keys $ distancesFromStart' m

timeSaved :: RaceTrack -> WallClip -> Int
timeSaved m (p, _, e) =
  let newTime = 2 + distancesFromStart' m M.! p + distancesFromEnd' m M.! e
      oldTime = glitchlessRecord m
   in oldTime - newTime

-- >>> allSavings <$> loadInput "example.txt"
-- [2,4,4,2,4,8,2,2,4,2,4,12,10,8,6,4,2,64,40,4,2,38,8,10,12,20,36,2,4,6,8,4,4,12,4,4,2,2,2,2,2,2,4,4]
allSavings :: RaceTrack -> [Int]
allSavings m = filter (> 0) $ map (timeSaved m) $ allWallClips m

-- >>> allSavingsCounts <$> loadInput "example.txt"
-- fromList [(2,14),(4,14),(6,2),(8,4),(10,2),(12,3),(20,1),(36,1),(38,1),(40,1),(64,1)]
allSavingsCounts :: RaceTrack -> M.Map Int Int
allSavingsCounts m = M.fromListWith (+) $ map (,1) $ allSavings m

part1 :: RaceTrack -> Int
part1 = sum . map snd . filter ((>= 100) . fst) . M.toList . allSavingsCounts

findCheats :: RaceTrack -> Int -> Coord2D -> S.Set Cheat
findCheats m threshold p@(x, y) = S.map (p,) $ S.filter ((>= threshold) . saving) potentialEnds
  where
    distance (x', y') = abs (x - x') + abs (y - y')
    withinDistance p' = distance p' <= 20
    potentialEnds = S.filter withinDistance $ M.keysSet $ distancesFromEnd' m
    newTimeVia p' = distance p' + distancesFromStart' m M.! p + distancesFromEnd' m M.! p'
    saving p' = glitchlessRecord m - newTimeVia p'

-- >>> S.size . allCheats 74 <$> loadInput "example.txt"
-- 7
allCheats :: Int -> RaceTrack -> S.Set Cheat
allCheats threshold m = S.unions $ S.map (findCheats m threshold) $ M.keysSet $ distancesFromStart' m

part2 :: RaceTrack -> Int
part2 = S.size . allCheats 100
