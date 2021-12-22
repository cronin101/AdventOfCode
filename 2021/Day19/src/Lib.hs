{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

module Lib (loadInput, solve, largestDistance, knownBeaconCount) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.Function (on)
import qualified Data.IntMap.Lazy as IM
import Data.List (find, group, maximumBy, partition, sort, sortBy, transpose)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import qualified Data.Set as S

newtype Coord3D = C3D (Int, Int, Int)
  deriving (Show, Eq, Ord)

instance Num Coord3D where
  C3D (x, y, z) + C3D (x', y', z') = C3D (x + x', y + y', z + z')
  C3D (x, y, z) - C3D (x', y', z') = C3D (x - x', y - y', z - z')
  (*) = error "undef"
  abs = error "undef"
  signum = error "undef"
  fromInteger = error "undef"
  negate (C3D (x, y, z)) = C3D (- x, - y, - z)

data Scanner = Scanner
  { s_id :: Int,
    knownLocation :: Maybe Coord3D,
    knownRotation :: Maybe Int,
    knownBeacons :: Maybe (S.Set Coord3D),
    beacons :: [S.Set Coord3D]
  }
  deriving (Show)

type ScannersById = IM.IntMap Scanner

-- >>> A.parseOnly parseCoord3D "-1,-1,1"
-- Right (-1,-1,1)
parseCoord3D :: A.Parser Coord3D
parseCoord3D = do
  [x, y, z] <- A.sepBy1 (A.signed A.decimal) ","
  return $ C3D (x, y, z)

-- >>> A.parseOnly parseScanner "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401"
-- Right (Scanner {id = 0, knownLocation = Nothing, beacons = [(404,-588,-901),(528,-643,409),(-838,591,734),(390,-675,-793),(-537,-823,-458),(-485,-357,347),(-345,-311,381),(-661,-816,-575),(-876,649,763),(-618,-824,-621),(553,345,-567),(474,580,667),(-447,-329,318),(-584,868,-557),(544,-627,-890),(564,392,-477),(455,729,728),(-892,524,684),(-689,845,-530),(423,-701,434),(7,-33,-71),(630,319,-379),(443,580,662),(-789,900,-551),(459,-707,401)]})
parseScanner :: A.Parser Scanner
parseScanner = do
  "--- scanner "
  id <- A.decimal
  " ---"
  A.endOfLine
  beacons <- A.sepBy1 parseCoord3D A.endOfLine
  return $ Scanner id Nothing Nothing Nothing (map S.fromList $ transpose $ map rotationsOfPoint beacons)

parseScanners :: A.Parser [Scanner]
parseScanners = do
  A.sepBy1 parseScanner $ A.count 2 A.endOfLine

loadInput :: [Char] -> IO ScannersById
loadInput fileName =
  (\scannersById -> IM.insert 0 ((scannersById IM.! 0) {knownLocation = Just $ C3D (0, 0, 0), knownRotation = Just 0, knownBeacons = Just ((head . beacons) (scannersById IM.! 0))}) scannersById)
    . IM.fromList
    . map (\s -> (s_id s, s))
    . fromRight
      []
    . A.parseOnly
      parseScanners
    <$> BSC.readFile
      ("src/" ++ fileName)

tryMatchScanners :: Scanner -> Scanner -> Maybe Scanner
tryMatchScanners a b = case knownLocation a of
  Nothing -> Nothing
  Just location
    | overlapCount < 12 -> Nothing
    | otherwise -> Just b {knownLocation = Just offsetToKnown, knownRotation = Just rotationId, knownBeacons = Just ({--S.union (fromJust $ knownBeacons a) $--} S.map (+ offsetToKnown) (beacons b !! rotationId))}
    where
      (rotationId, (offsetToKnown, overlapCount)) = mostMatches [(rid,) $ mostCommonWithCount [a - b | a <- S.toList $fromJust $ knownBeacons a, b <- S.toList $ beacons b !! rid] | rid <- rids]
      rids = [0 .. length (beacons b) - 1]
      mostMatches = maximumBy (compare `on` (snd . snd))
      mostCommonWithCount = maximumBy (compare `on` snd) . map (\cs -> (head cs, length cs)) . group . sort

rotationsOfPoint :: Coord3D -> [Coord3D]
rotationsOfPoint point = map (\r -> r point) rotations

solve :: ScannersById -> ScannersById
solve scannersById
  | all (isJust . knownLocation) (IM.elems scannersById) = scannersById
  | otherwise = solve $ IM.unions (map (\m -> IM.singleton (s_id m) m) matched ++ [scannersById])
  where
    matched = mapMaybe (uncurry tryMatchScanners) pairs
    pairs = [(a, b) | a <- solvedScanners, b <- unsolvedScanners]
    (solvedScanners, unsolvedScanners) = partition (isJust . knownLocation) $ IM.elems scannersById

-- >>> knownBeaconCount . solve <$> loadInput "example.txt"
-- 79
knownBeaconCount :: ScannersById -> Int
knownBeaconCount = S.size . S.unions . mapMaybe knownBeacons . IM.elems . solve

manhattanDistance :: Coord3D -> Coord3D -> Int
manhattanDistance (C3D (x, y, z)) (C3D (x', y', z')) = abs (x - x') + abs (y - y') + abs (z - z')

-- >>> largestDistance . solve <$> loadInput "example.txt"
-- 3621
largestDistance :: ScannersById -> Int
largestDistance scannersById = maximum [manhattanDistance a b | a <- positions, b <- positions, a /= b]
  where
    positions = mapMaybe knownLocation $ IM.elems scannersById

-- >>> length $ map (\r -> r (C3D(1, 2, 3))) rotations
-- 24
rotations :: [Coord3D -> Coord3D]
rotations =
  -- x facing x
  [ \(C3D (x, y, z)) -> C3D (x, y, z),
    \(C3D (x, y, z)) -> C3D (x, - z, y),
    \(C3D (x, y, z)) -> C3D (x, - y, - z),
    \(C3D (x, y, z)) -> C3D (x, z, - y),
    -- x facing -x
    \(C3D (x, y, z)) -> C3D (- x, - y, z),
    \(C3D (x, y, z)) -> C3D (- x, - z, - y),
    \(C3D (x, y, z)) -> C3D (- x, y, - z),
    \(C3D (x, y, z)) -> C3D (- x, z, y),
    -- x facing y
    \(C3D (x, y, z)) -> C3D (- z, x, - y),
    \(C3D (x, y, z)) -> C3D (y, x, - z),
    \(C3D (x, y, z)) -> C3D (z, x, y),
    \(C3D (x, y, z)) -> C3D (- y, x, z),
    -- x facing -y
    \(C3D (x, y, z)) -> C3D (z, - x, - y),
    \(C3D (x, y, z)) -> C3D (y, - x, z),
    \(C3D (x, y, z)) -> C3D (- z, - x, y),
    \(C3D (x, y, z)) -> C3D (- y, - x, - z),
    -- x facing z
    \(C3D (x, y, z)) -> C3D (- y, - z, x),
    \(C3D (x, y, z)) -> C3D (z, - y, x),
    \(C3D (x, y, z)) -> C3D (y, z, x),
    \(C3D (x, y, z)) -> C3D (- z, y, x),
    -- x facing -z
    \(C3D (x, y, z)) -> C3D (z, y, - x),
    \(C3D (x, y, z)) -> C3D (- y, z, - x),
    \(C3D (x, y, z)) -> C3D (- z, - y, - x),
    \(C3D (x, y, z)) -> C3D (y, - z, - x)
  ]
