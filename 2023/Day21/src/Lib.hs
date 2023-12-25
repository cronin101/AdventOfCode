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

import Control.Arrow (Arrow (first, second))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)

data Square = Garden | Rock | Origin deriving (Eq)

instance Show Square where
  show :: Square -> String
  show Garden = "."
  show Rock = "#"
  show Origin = "S"

instance Read Square where
  readsPrec :: Int -> ReadS Square
  readsPrec _ [] = []
  readsPrec _ (c : rest) =
    [ case c of
        '.' -> (Garden, rest)
        '#' -> (Rock, rest)
        'S' -> (Origin, rest)
        _ -> error "Invalid square"
    ]

type Coord2D = (Int, Int)

type Map = M.Map Coord2D Square

type State = (Map, M.Map Coord2D [Coord2D], S.Set Coord2D)

parseSquare :: A.Parser Square
parseSquare = read . pure <$> A.satisfy (\c -> c `elem` (".#S" :: String))

-- >>> A.parseOnly parseRow ".##..S####."
-- Right [((0,0),.),((1,0),#),((2,0),#),((3,0),.),((4,0),.),((5,0),S),((6,0),#),((7,0),#),((8,0),#),((9,0),#),((10,0),.)]
parseRow :: A.Parser [(Coord2D, Square)]
parseRow = zipWith (\x s -> ((x, 0), s)) [0 ..] <$> A.many1 parseSquare

parseMap :: A.Parser Map
parseMap = M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)

-- >>> loadInput "example.txt"
-- fromList [((0,0),.),((0,1),.),((0,2),.),((0,3),.),((0,4),.),((0,5),.),((0,6),.),((0,7),.),((0,8),.),((0,9),.),((0,10),.),((1,0),.),((1,1),#),((1,2),#),((1,3),.),((1,4),#),((1,5),#),((1,6),.),((1,7),.),((1,8),#),((1,9),.),((1,10),.),((2,0),.),((2,1),#),((2,2),#),((2,3),.),((2,4),#),((2,5),#),((2,6),.),((2,7),#),((2,8),#),((2,9),.),((2,10),.),((3,0),.),((3,1),.),((3,2),.),((3,3),.),((3,4),.),((3,5),.),((3,6),.),((3,7),.),((3,8),#),((3,9),.),((3,10),.),((4,0),.),((4,1),.),((4,2),#),((4,3),.),((4,4),.),((4,5),.),((4,6),#),((4,7),#),((4,8),.),((4,9),.),((4,10),.),((5,0),.),((5,1),#),((5,2),.),((5,3),.),((5,4),#),((5,5),S),((5,6),.),((5,7),.),((5,8),#),((5,9),#),((5,10),.),((6,0),.),((6,1),#),((6,2),#),((6,3),.),((6,4),.),((6,5),#),((6,6),#),((6,7),.),((6,8),#),((6,9),#),((6,10),.),((7,0),.),((7,1),.),((7,2),#),((7,3),#),((7,4),.),((7,5),#),((7,6),.),((7,7),.),((7,8),.),((7,9),#),((7,10),.),((8,0),.),((8,1),#),((8,2),#),((8,3),#),((8,4),.),((8,5),#),((8,6),.),((8,7),#),((8,8),.),((8,9),.),((8,10),.),((9,0),.),((9,1),#),((9,2),#),((9,3),.),((9,4),#),((9,5),#),((9,6),.),((9,7),.),((9,8),#),((9,9),#),((9,10),.),((10,0),.),((10,1),.),((10,2),.),((10,3),.),((10,4),.),((10,5),.),((10,6),.),((10,7),.),((10,8),.),((10,9),.),((10,10),.)]
loadInput :: [Char] -> IO Map
loadInput = (fromRight M.empty . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)

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

printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [(\s -> if length s == 1 then "  " ++ s else if length s == 2 then ' ' : s else s) $ fromMaybe "." (M.lookup (x, y) m) | x <- [minX .. maxX]] | y <- reverse [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds m

printMap :: Map -> String
printMap = printGrid . M.map show

initialize :: Map -> State
initialize m = (m', a, S.singleton origin)
  where
    m' = M.insert origin Garden m
    a = M.mapWithKey (\(x, y) _ -> filter (\c -> inBounds c m' && (m' M.! c == Garden)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]) m'
    origin = fst . head . M.toList $ M.filter (== Origin) m

initializeInfinite :: Map -> State
initializeInfinite m = (m', a, S.singleton origin)
  where
    wrapX x = x `mod` (maxX + 1)
    wrapY y = y `mod` (maxY + 1)
    (_, (maxX, maxY)) = bounds a
    m' = M.insert origin Garden m
    a = M.mapWithKey (\(x, y) _ -> filter (\(x', y') -> m' M.! (wrapX x', wrapY y') == Garden) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]) m'
    origin = fst . head . M.toList $ M.filter (== Origin) m

step :: State -> State
step (m, a, f) = (m, a, f')
  where
    f' = S.fromList $ concatMap (a M.!) f

stepInfinite :: State -> State
stepInfinite (m, a, f) = (m, a, f')
  where
    f' =
      S.filter (\(x, y) -> m M.! (wrapX x, wrapY y) == Garden) $
        S.unions
          [ S.map (second (+ 1)) f,
            S.map (second (subtract 1)) f,
            S.map (first (+ 1)) f,
            S.map (first (subtract 1)) f
          ]
    wrapX x = x `mod` (maxX + 1)
    wrapY y = y `mod` (maxY + 1)
    (_, (maxX, maxY)) = bounds a

printState :: State -> String
printState (m, _, f) = printGrid $ M.mapWithKey (\c s -> if S.member c f then "O" else show s) m

-- >>> part1 6 <$> loadInput "example.txt"
-- 16
part1 :: Int -> Map -> Int
part1 target m = S.size $ (\(_, _, f) -> f) $ iterate stepInfinite (initializeInfinite m) !! target

sizes :: Map -> [Int]
sizes m = map (\(_, _, f) -> S.size f) (iterate stepInfinite (initializeInfinite m))

-- >>> intervals [2, 9, 20, 35, 54]
-- [7,11,15,19]
intervals :: [Int] -> [Int]
intervals xs = zipWith subtract xs (tail xs)

-- >>> isQuadratic 3 [2, 9, 20, 35, 54]
-- True

-- >>> isQuadratic 3  (2: [1..])
-- False

isQuadratic :: Int -> [Int] -> Bool
isQuadratic l xs = not (null secondIntervals) && all (== 0) secondIntervals
  where
    secondIntervals = (take l . intervals . intervals . intervals) xs

-- >>> take 10 $ withOffsetAndPeriod [1..] (5, 10)
-- [5,15,25,35,45,55,65,75,85,95]
withOffsetAndPeriod :: [Int] -> (Int, Int) -> [Int]
withOffsetAndPeriod xs (offset, period) = takeEvery period $ drop (offset - 1) xs
  where
    takeEvery n = map snd . filter (\(i, _) -> i `mod` n == 0) . zip [0 ..]

findPotentialQuadraticCycles :: Int -> [Int] -> [(Int, Int)]
findPotentialQuadraticCycles limit vals = [c | c <- coefficients, isQuadratic 2 $ withOffsetAndPeriod vals c]
  where
    coefficients = sortBy (compare `on` uncurry (+)) [(offset, period) | offset <- [1 .. limit], period <- [1 .. limit]]

-- >>> 4 `termOfQuadraticSequence` [2, 9, 20, 35, 54]
-- 35
termOfQuadraticSequence :: Int -> [Int] -> Int
termOfQuadraticSequence n vals = a * (n ^ 2) + b * n + c
  where
    (a, b, c) = quadraticCoefficients vals

-- >>> quadraticCoefficients [2, 9, 20, 35, 54]
-- (2,1,-1)

-- >>> quadraticCoefficients [6, 15, 28, 45, 66]
-- (2,3,1)

quadraticCoefficients :: [Int] -> (Int, Int, Int)
quadraticCoefficients vals = trace ("(a, b, c) = " ++ show (a, b, c)) (a, b, c)
  where
    diffs = intervals vals
    diffs' = intervals diffs
    a = head diffs' `div` 2
    linearPart = zipWith (\n v -> v - (a * (n ^ 2))) [1 ..] vals
    b = head $ intervals linearPart
    c = head linearPart - b

-- This needed some Reddit hinting also. With the insight that it is a quadratic sequence that lines up with the goal, it isn't too bad.
part2 :: Map -> Int
part2 m =
  let stepsTaken = 26501365
      ((minX, minY), (maxX, maxY)) = bounds m
      maxPeriod = max (1 + maxY - minY) (1 + maxX - minX)
      sizes' = sizes m
      (offset, period) = head $ filter (\(o, p) -> (stepsTaken - o) `mod` p == 0) $ findPotentialQuadraticCycles maxPeriod sizes'
      iterations = 4
      needed = trace ("(o,p) = " ++ show (offset, period)) (offset + (period * (iterations - 1)))
      sizeLookup = M.fromList $ take needed $ zip [1 ..] $ drop 1 sizes'
      vals = map (\k -> sizeLookup M.! (offset + (period * k))) [0 .. iterations - 1]
      n = 1 + ((stepsTaken - offset) `div` period)
   in n `termOfQuadraticSequence` vals
