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

-- Coordinates on the race track grid, representing positions in the code path.
type Coord2D = (Int, Int)

-- A daring glitch maneuver through a wall: start, wall to pass, and exit point.
-- (Start, Wall, End)
type WallClip = (Coord2D, Coord2D, Coord2D)

-- A forbidden shortcut from one point to another, bypassing the normal race path.
-- (Start, End)
type Cheat = (Coord2D, Coord2D)

-- The basic building blocks of our race track: walls, the starting line, and the finish line.
data Tile = Wall | Start | End deriving (Eq)

-- The entire race track layout, including special tiles and distances to key points.
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

-- Render the race track with its pathways and obstacles, revealing the distances to victory.
printRaceTrack :: RaceTrack -> String
printRaceTrack (RaceTrack (maxX, maxY) m _ distancesFromStart distancesFromEnd) =
  let longestDistanceFromStartStringLength = maximum $ map (length . show) $ M.elems distancesFromStart
      longestDistanceFromEndStringLength = maximum $ map (length . show) $ M.elems distancesFromEnd
      longestDistanceStringLength = max longestDistanceFromStartStringLength longestDistanceFromEndStringLength
      pad str = let padding = replicate (longestDistanceStringLength - length str) ' ' in padding ++ str ++ " "
      minDistanceToExtreme = M.unionWith min distancesFromStart distancesFromEnd
      showTile (x, y) = pad $ fromMaybe "." $ M.lookup (x, y) $ M.unions [M.map show m, M.map show minDistanceToExtreme]
   in intercalate "\n" ([concat [showTile (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]])

-- Decode a single tile character from the race track map, deciphering walls and starting points.
parseTile :: A.Parser Tile
parseTile = (Wall <$ "#") <|> (Start <$ "S") <|> (End <$ "E")

-- Parse a row of the race track map into coordinates and tiles, mapping out the treacherous path.
-- >>> A.parseOnly parseRow "#S#...#.#.#...#"
-- Right [((0,0),#),((1,0),S),((2,0),#),((6,0),#),((8,0),#),((10,0),#),((14,0),#)]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = map (second fromJust) . filter (isJust . snd) . zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (Just <$> parseTile <|> Nothing <$ ".")

-- Assemble the entire race track from the map, plotting walls and calculating distances.
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

-- Load the race track map from a file, preparing the stage for the race of a lifetime.
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

-- Explore the neighboring positions from a given point, seeking possible moves.
neighbours :: Coord2D -> [Coord2D]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- The best possible race time without resorting to any underhanded tricks.
-- >>> glitchlessRecord <$> loadInput "example.txt"
-- 84
glitchlessRecord :: RaceTrack -> Int
glitchlessRecord m = distancesFromStart' m M.! end' m

-- Navigate the race track using Dijkstra's algorithm, charting the shortest paths.
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

-- Identify all the walls near a point that could be glitched through, plotting potential cheats.
wallClipsForPoint :: RaceTrack -> Coord2D -> [WallClip]
wallClipsForPoint m p = [(p, w, e) | p `S.notMember` walls, w <- neighbours p, w `S.member` walls, e <- neighbours w, e `M.member` distancesFromEnd' m, e /= p]
  where
    walls = M.keysSet $ M.filter (== Wall) $ tiles' m

-- Compile a complete list of all cheating opportunities across the race track.
-- >>> allWallClips <$> loadInput "example.txt"
-- [((1,2),(2,2),(3,2)),((1,2),(2,2),(2,1)), ... ]
allWallClips :: RaceTrack -> [WallClip]
allWallClips m = concatMap (wallClipsForPoint m) $ M.keys $ distancesFromStart' m

-- Calculate how much time a racer would save by exploiting a wall clip at a specific point.
timeSaved :: RaceTrack -> WallClip -> Int
timeSaved m (p, _, e) =
  let newTime = 2 + distancesFromStart' m M.! p + distancesFromEnd' m M.! e
      oldTime = glitchlessRecord m
   in oldTime - newTime

-- Gather all the time savings possible from every cheat, tempting racers with forbidden speed.
-- >>> allSavings <$> loadInput "example.txt"
-- [2,4,4,2,4,8,2,2,4,2,4,12,10,8,6,4,2,64,40,4,2,38,8,10,12,20,36,2,4,6,8,4,4,12,4,4,2,2,2,2,2,2,4,4]
allSavings :: RaceTrack -> [Int]
allSavings m = filter (> 0) $ map (timeSaved m) $ allWallClips m

-- Tally up the number of cheats available for each amount of time saved.
-- >>> allSavingsCounts <$> loadInput "example.txt"
-- fromList [(2,14),(4,14),(6,2),(8,4),(10,2),(12,3),(20,1),(36,1),(38,1),(40,1),(64,1)]
allSavingsCounts :: RaceTrack -> M.Map Int Int
allSavingsCounts m = M.fromListWith (+) $ map (,1) $ allSavings m

-- Count how many cheats would let a racer save at least 100 picoseconds under old rules.
part1 :: RaceTrack -> Int
part1 = sum . map snd . filter ((>= 100) . fst) . M.toList . allSavingsCounts

-- Find all possible cheats starting from a point that save at least a given threshold.
findCheats :: RaceTrack -> Int -> Coord2D -> S.Set Cheat
findCheats m threshold p@(x, y) = S.map (p,) $ S.filter ((>= threshold) . saving) potentialEnds
  where
    distance (x', y') = abs (x - x') + abs (y - y')
    withinDistance p' = distance p' <= 20
    potentialEnds = S.filter withinDistance $ M.keysSet $ distancesFromEnd' m
    newTimeVia p' = distance p' + distancesFromStart' m M.! p + distancesFromEnd' m M.! p'
    saving p' = glitchlessRecord m - newTimeVia p'

-- Discover every possible cheat across the track that saves at least the given time under new rules.
-- >>> S.size . allCheats 74 <$> loadInput "example.txt"
-- 7
allCheats :: Int -> RaceTrack -> S.Set Cheat
allCheats threshold m = S.unions $ S.map (findCheats m threshold) $ M.keysSet $ distancesFromStart' m

-- Calculate the total number of cheats saving at least 100 picoseconds with extended cheat duration.
part2 :: RaceTrack -> Int
part2 = S.size . allCheats 100
