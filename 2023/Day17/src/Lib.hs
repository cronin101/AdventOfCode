{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    steps,
    printSearchState,
    printPath,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (intercalate, tails)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

data CardinalDirection = North | East | South | West deriving (Show, Eq, Ord)

type Coord2D = (Int, Int)

type Position = (Coord2D, CardinalDirection)

type Grid = M.Map Coord2D Int

type OutputGrid = M.Map Position (Int, Maybe Position)

type SearchState = (S.Set Position, OutputGrid)

-- >>> A.parseOnly parseRow "2413432311323"
-- Right [((0,0),2),((1,0),4),((2,0),1),((3,0),3),((4,0),4),((5,0),3),((6,0),2),((7,0),3),((8,0),1),((9,0),1),((10,0),3),((11,0),2),((12,0),3)]
parseRow :: A.Parser [(Coord2D, Int)]
parseRow = zipWith (\x d -> ((x, 0), read $ pure d)) [0 ..] <$> A.many1 A.digit

parseGrid :: A.Parser Grid
parseGrid = M.fromList . concat . zipWith (\y r -> map (\((x, _), e) -> ((x, y), e)) r) [0 ..] . reverse <$> parseRow `A.sepBy1` A.endOfLine

-- >>> loadInput "example.txt"
-- fromList [((0,0),4),((0,1),2),((0,2),1),((0,3),4),((0,4),4),((0,5),3),((0,6),4),((0,7),1),((0,8),4),((0,9),3),((0,10),3),((0,11),3),((0,12),2),((1,0),3),((1,1),5),((1,2),2),((1,3),5),((1,4),6),((1,5),6),((1,6),4),((1,7),4),((1,8),5),((1,9),4),((1,10),2),((1,11),2),((1,12),4),((2,0),2),((2,1),4),((2,2),2),((2,3),6),((2,4),5),((2,5),3),((2,6),5),((2,7),3),((2,8),4),((2,9),4),((2,10),5),((2,11),1),((2,12),1),((3,0),2),((3,1),6),((3,2),4),((3,3),4),((3,4),4),((3,5),7),((3,6),7),((3,7),8),((3,8),6),((3,9),6),((3,10),5),((3,11),5),((3,12),3),((4,0),6),((4,1),5),((4,2),6),((4,3),6),((4,4),9),((4,5),8),((4,6),8),((4,7),5),((4,8),6),((4,9),5),((4,10),2),((4,11),4),((4,12),4),((5,0),7),((5,1),4),((5,2),8),((5,3),7),((5,4),6),((5,5),7),((5,6),7),((5,7),9),((5,8),5),((5,9),8),((5,10),4),((5,11),5),((5,12),3),((6,0),4),((6,1),8),((6,2),6),((6,3),9),((6,4),7),((6,5),7),((6,6),6),((6,7),8),((6,8),7),((6,9),5),((6,10),5),((6,11),3),((6,12),2),((7,0),6),((7,1),8),((7,2),8),((7,3),9),((7,4),9),((7,5),9),((7,6),9),((7,7),7),((7,8),8),((7,9),8),((7,10),6),((7,11),5),((7,12),3),((8,0),5),((8,1),8),((8,2),6),((8,3),8),((8,4),8),((8,5),7),((8,6),8),((8,7),9),((8,8),6),((8,9),4),((8,10),5),((8,11),3),((8,12),1),((9,0),5),((9,1),7),((9,2),5),((9,3),6),((9,4),6),((9,5),9),((9,6),7),((9,7),8),((9,8),7),((9,9),5),((9,10),4),((9,11),5),((9,12),1),((10,0),5),((10,1),7),((10,2),5),((10,3),4),((10,4),8),((10,5),6),((10,6),7),((10,7),4),((10,8),5),((10,9),4),((10,10),2),((10,11),6),((10,12),3),((11,0),3),((11,1),3),((11,2),6),((11,3),5),((11,4),8),((11,5),5),((11,6),6),((11,7),5),((11,8),3),((11,9),5),((11,10),5),((11,11),2),((11,12),2),((12,0),3),((12,1),5),((12,2),3),((12,3),3),((12,4),7),((12,5),3),((12,6),6),((12,7),4),((12,8),6),((12,9),2),((12,10),4),((12,11),3),((12,12),3)]
loadInput :: [Char] -> IO Grid
loadInput = (fromRight M.empty . A.parseOnly parseGrid <$>) . BSC.readFile . ("src/" ++)

-- >>> bounds <$> loadInput "example.txt"
-- ((0,0),(12,12))
bounds :: M.Map Coord2D a -> (Coord2D, Coord2D)
bounds g
  | M.null g = ((0, 0), (0, 0))
  | otherwise = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    (xs, ys) = unzip $ M.keys g

-- >>> inBounds  (6,6) <$> loadInput "example.txt"
-- True
inBounds :: Coord2D -> M.Map Coord2D a -> Bool
inBounds (x, y) g = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    ((minX, minY), (maxX, maxY)) = bounds g

-- >>> (\g -> orthogonalBursts g (1, 3) ((12, 0), North)) <$> loadInput "example.txt"
-- [(((9,0),West),13),(((10,0),West),8),(((11,0),West),3)]

-- >>> (\g -> orthogonalBursts g (4, 10) ((12, 0), North)) <$> loadInput "example.txt"
-- [(((2,0),West),45),(((3,0),West),43),(((4,0),West),41),(((5,0),West),35),(((6,0),West),28),(((7,0),West),24),(((8,0),West),18)]
orthogonalBursts g (minB, maxB) ((x, y), d) = endsWithCosts
  where
    endsWithCosts = map (\p@(p' : _) -> (p', sum $ map ((g M.!) . fst) p)) $ filter ((`M.member` g) . fst . head) paths
    paths = concat [bursts vector | vector <- orthoganalVectors]
    orthoganalVectors = let (dx', dy') = directionToVector d in filter (\(dx, dy) -> ((dx * dx') + (dy * dy')) == 0) vectors
    vectors = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx == 0 || dy == 0, dx /= dy, directionToVector d /= (-dx, -dy)]
    bursts v@(dx, dy) = take (1 + maxB - minB) $ tails $ [((x + n' * dx, y + n' * dy), vectorToDirection v) | n' <- [maxB, maxB - 1 .. 1]]

directionToVector :: CardinalDirection -> Coord2D
directionToVector North = (0, 1)
directionToVector East = (1, 0)
directionToVector South = (0, -1)
directionToVector West = (-1, 0)

vectorToDirection :: Coord2D -> CardinalDirection
vectorToDirection (0, 1) = North
vectorToDirection (1, 0) = East
vectorToDirection (0, -1) = South
vectorToDirection (-1, 0) = West
vectorToDirection _ = error "Not a unit vector"

steps :: Grid -> (Int, Int) -> [SearchState]
steps g burstRange = map fst $ takeWhile (uncurry (/=)) $ zip steps' (drop 1 steps')
  where
    goal = (maxX, minY)
    ((_, minY), (maxX, _)) = bounds g
    allOrthogonals = M.fromSet (\p -> M.fromList $ map (second (,Just p)) $ orthogonalBursts g burstRange p) allPositions
    allPositions = S.unions $ S.map (\c -> S.fromList $ map (c,) [North, South, East, West]) $ M.keysSet g
    steps' = iterate step (firstFrontier, initialState)
    firstFrontier = S.fromList $ map (goal,) [North, South, East, West]
    initialState = M.fromList $ map (\d -> ((goal, d), (g M.! goal, Nothing))) [North, South, East, West]
    step state@(frontier, outputGrid)
      | S.null frontier = state
      | otherwise = (frontier', outputGrid')
      where
        neighboursWithCost =
          M.unionsWith min $
            map (\f -> M.map (first (+ (fst $ outputGrid M.! f))) $ allOrthogonals M.! f) $
              S.toList frontier
        outputGrid' = M.unionWith min outputGrid neighboursWithCost
        frontier' = S.filter (\c -> M.lookup c outputGrid /= M.lookup c outputGrid') $ M.keysSet neighboursWithCost

printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [(\s -> if length s == 1 then "  " ++ s else if length s == 2 then ' ' : s else s) $ fromMaybe "." (M.lookup (x, y) m) | x <- [0 .. maxX]] | y <- reverse [0 .. maxY]])
  where
    (_, (maxX, maxY)) = bounds m

printSearchState :: SearchState -> String
printSearchState (_, solved) = printGrid (M.map (show . fst) $ M.mapKeysWith min fst solved)

-- >>>  readPath ((0, 12), North) . snd . last . steps  <$> loadInput "example.txt"
-- [((0,12),North),((0,11),West),((1,11),West),((2,11),South),((2,12),West),((3,12),West),((4,12),West),((5,12),North),((5,11),West),((6,11),South),((6,12),West),((7,12),West),((8,12),West),((9,12),North),((9,11),North),((9,10),West),((10,10),North),((10,9),North),((10,8),West),((11,8),North),((11,7),North),((11,6),North),((11,5),West),((12,5),North),((12,4),North),((12,3),North),((12,2),East),((11,2),North),((11,1),North),((11,0),West)]
readPath :: Position -> OutputGrid -> [Position]
readPath c@((x, y), d) g = case M.lookup c g of
  Nothing -> []
  Just (_, Nothing) -> []
  Just (_, Just c'@((x', y'), _)) -> init [((x'', y''), d) | x'' <- (if x' < x then reverse else id) [(min x x') .. (max x x')], y'' <- (if y' < y then reverse else id) [(min y y') .. (max y y')]] ++ readPath c' g

-- >>> printPath . last . steps <$> loadInput "example.txt"
-- "  v  .  >  >  >  v  >  >  >  v  .  .  .\n  >  >  ^  .  .  >  ^  .  .  v  .  .  .\n  .  .  .  .  .  .  .  .  .  >  v  .  .\n  .  .  .  .  .  .  .  .  .  .  v  .  .\n  .  .  .  .  .  .  .  .  .  .  >  v  .\n  .  .  .  .  .  .  .  .  .  .  .  v  .\n  .  .  .  .  .  .  .  .  .  .  .  v  .\n  .  .  .  .  .  .  .  .  .  .  .  >  v\n  .  .  .  .  .  .  .  .  .  .  .  .  v\n  .  .  .  .  .  .  .  .  .  .  .  .  v\n  .  .  .  .  .  .  .  .  .  .  .  v  <\n  .  .  .  .  .  .  .  .  .  .  .  v  .\n  .  .  .  .  .  .  .  .  .  .  .  >  ."
printPath :: SearchState -> String
printPath (_, solvedGrid) = printGrid $ M.map awayFrom $ M.fromList $ readPath ((minX, maxY), North) solvedGrid
  where
    awayFrom North = "v"
    awayFrom South = "^"
    awayFrom East = "<"
    awayFrom West = ">"
    ((minX, _), (_, maxY)) = bounds $ M.mapKeys fst solvedGrid

heatLossWithBurstRange :: (Int, Int) -> Grid -> Int
heatLossWithBurstRange burstRange g = minimum (map (fst . snd) solutionsFromStart) - startCost
  where
    solutionsFromStart = filter ((== start) . fst . fst) (M.toList solvedGrid)
    (_, solvedGrid) = last $ steps g burstRange
    startCost = g M.! start
    start = (minX, maxY)
    ((minX, _), (_, maxY)) = bounds g

-- >>> part1 <$> loadInput "example.txt"
-- 102
part1 :: Grid -> Int
part1 = heatLossWithBurstRange (1, 3)

-- >>> part2 <$> loadInput "example.txt"
-- 94
part2 :: Grid -> Int
part2 = heatLossWithBurstRange (4, 10)
