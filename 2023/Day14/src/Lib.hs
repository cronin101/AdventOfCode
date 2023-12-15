{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (elemIndex)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as S

data Rock = Rounded | Cube deriving (Show, Eq)

type Coord2D = (Int, Int)

type Rocks = S.Set Coord2D

data Platform = Platform
  { bounds :: (Coord2D, Coord2D),
    roundRocks :: Rocks,
    cubeRocks :: Rocks
  }
  deriving (Show, Eq, Ord)

-- >>> A.parseOnly (A.many1 parseRock) "O....#...."
-- Right [Just Rounded,Nothing,Nothing,Nothing,Nothing,Just Cube,Nothing,Nothing,Nothing,Nothing]
parseRock :: A.Parser (Maybe Rock)
parseRock = "O" $> Just Rounded <|> "#" $> Just Cube <|> "." $> Nothing

-- >>> A.parseOnly parseRow "OO.#O....O"
-- Right (Platform {bounds = ((0,0),(9,0)), roundRocks = fromList [(0,0),(1,0),(4,0),(9,0)], cubeRocks = fromList [(3,0)]})
parseRow :: A.Parser Platform
parseRow = do
  rocks <- A.many1 parseRock
  let rocks' = zipWith (\x r -> ((x, 0), r)) [0 ..] rocks
  let roundR = S.fromList $ map fst $ filter (\(_, r) -> r == Just Rounded) rocks'
  let cubeR = S.fromList $ map fst $ filter (\(_, r) -> r == Just Cube) rocks'
  return $ Platform ((0, 0), (length rocks - 1, 0)) roundR cubeR

parsePlatform :: A.Parser Platform
parsePlatform = do
  rows <- reverse <$> parseRow `A.sepBy` A.endOfLine
  let roundR = zipWith (\p y -> S.map (\(x, _) -> (x, y)) $ roundRocks p) rows [0 ..]
  let cubeR = zipWith (\p y -> S.map (\(x, _) -> (x, y)) $ cubeRocks p) rows [0 ..]
  let ((_, _), (cols, _)) = bounds $ head rows
  return $ Platform ((0, 0), (cols, length rows - 1)) (S.unions roundR) (S.unions cubeR)

-- >>> loadInput "example.txt"
-- Platform {bounds = ((0,0),(9,9)), roundRocks = fromList [(0,4),(0,6),(0,8),(0,9),(1,0),(1,5),(1,6),(2,0),(2,3),(2,8),(3,8),(4,6),(5,4),(6,3),(7,2),(7,5),(9,3),(9,6)], cubeRocks = fromList [(0,0),(0,1),(2,4),(3,6),(4,8),(5,0),(5,1),(5,3),(5,7),(5,9),(6,1),(6,7),(7,1),(7,4),(8,5),(9,4),(9,8)]}
loadInput :: [Char] -> IO Platform
loadInput = (fromRight (Platform ((0, 0), (0, 0)) S.empty S.empty) . A.parseOnly parsePlatform <$>) . BSC.readFile . ("src/" ++)

tiltFrame :: (Int, Int) -> Platform -> Platform
tiltFrame v@(dx, dy) p@(Platform (loBounds, hiBounds) roundR cubeR) = p {roundRocks = roundRocks'}
  where
    coordinateSelector = case v of (_, 0) -> fst; (0, _) -> snd; _ -> error "Invalid vector"
    iterations = drop 1 $ (if coordinateSelector v < 0 then id else reverse) [coordinateSelector loBounds .. coordinateSelector hiBounds]
    cubeRocksByI = M.unionsWith S.union $ map (\c -> M.singleton (coordinateSelector c) (S.singleton c)) $ S.toList cubeR
    roundRocksByI = M.unionsWith S.union $ map (\c -> M.singleton (coordinateSelector c) (S.singleton c)) $ S.toList roundR
    roundRocks' = foldl nextRocks roundR iterations
    nextRocks rocks i = S.union (rocks S.\\ didMove) moved
      where
        destinationI = i + coordinateSelector v
        toBlock = S.union (fromMaybe S.empty $ destinationI `M.lookup` cubeRocksByI) (fromMaybe S.empty $ destinationI `M.lookup` roundRocksByI)
        moved = S.map (\(x, y) -> (x + dx, y + dy)) (fromMaybe S.empty $ i `M.lookup` roundRocksByI) S.\\ toBlock
        didMove = S.map (\(x, y) -> (x - dx, y - dy)) moved

tiltPlatform :: (Int, Int) -> Platform -> Platform
tiltPlatform vector p = fst . head . dropWhile (uncurry (/=)) $ zip tilts (drop 1 tilts)
  where
    tilts = iterate (tiltFrame vector) p

tiltNorth :: Platform -> Platform
tiltNorth = tiltPlatform (0, 1)

tiltEast :: Platform -> Platform
tiltEast = tiltPlatform (1, 0)

tiltSouth :: Platform -> Platform
tiltSouth = tiltPlatform (0, -1)

tiltWest :: Platform -> Platform
tiltWest = tiltPlatform (-1, 0)

northLoad :: Platform -> Int
northLoad = sum . map ((+ 1) . snd) . S.toList . roundRocks

-- >>> part1 <$> loadInput "example.txt"
-- 136
part1 :: Platform -> Int
part1 = northLoad . tiltNorth

spin :: Platform -> Platform
spin = tiltEast . tiltSouth . tiltWest . tiltNorth

-- >>> northLoad . spinLongEnough <$> loadInput "example.txt"
-- 64
spinLongEnough :: Platform -> Platform
spinLongEnough p = iterate spin p' !! remaining
  where
    remaining = (targetSpins - start) `mod` cycleLength
    targetSpins = 1000000000
    (start, cycleLength, p') = findCycleStartAndLength p

-- >>> (\(s, l, _) -> (s, l)) . findCycleStartAndLength <$> loadInput "example.txt"
-- (3,7)
findCycleStartAndLength :: Platform -> (Int, Int, Platform)
findCycleStartAndLength p = (start, cycleLength, p')
  where
    cycleLength = 1 + fromJust (elemIndex p' $ drop 1 $ iterate spin p')
    (start, p') = findCycleStart p

-- >>> fst . findCycleStart <$> loadInput "example.txt"
-- 3
findCycleStart :: Platform -> (Int, Platform)
findCycleStart p = (fromJust $ elemIndex duplicate spins, duplicate)
  where
    spins = iterate spin p
    notSeen (counts, e) = M.lookup e counts /= Just 2
    incrementCountsWithElement (counts, _) e = (M.insertWith (+) e 1 counts, e)
    duplicate = snd . head . dropWhile notSeen $ scanl incrementCountsWithElement (M.empty, p) spins

-- >>> part2 <$> loadInput "example.txt"
-- 64
part2 :: Platform -> Int
part2 = northLoad . spinLongEnough
