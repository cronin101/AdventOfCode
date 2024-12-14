{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Monad (join)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (intercalate, minimumBy)
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S

type Coord2D = (Int, Int)

data Area = Area {bounds' :: (Coord2D, Coord2D), robots' :: M.Map Coord2D (S.Set Robot)}

instance Show Area where
  show = printGrid

data Robot = Robot {p' :: Coord2D, v' :: Coord2D}
  deriving (Show, Ord, Eq)

-- >>> A.parseOnly parseRobot "p=0,4 v=3,-3"
-- Right (Robot {p' = (0,4), v' = (3,-3)})
parseRobot :: A.Parser Robot
parseRobot = Robot <$> ("p=" *> parseCoord2D <* " v=") <*> parseCoord2D
  where
    parseCoord2D = (,) <$> (A.signed A.decimal <* ",") <*> A.signed A.decimal

toRobotMap :: [Robot] -> M.Map Coord2D (S.Set Robot)
toRobotMap = M.fromListWith S.union . map (\r -> (p' r, S.singleton r))

-- >>>  loadInput (11, 7) "example.txt"
-- 1.12.......
-- ...........
-- ...........
-- ......11.11
-- 1.1........
-- .........1.
-- .......1...
loadInput :: Coord2D -> [Char] -> IO Area
loadInput (w, h) = (Area ((0, 0), (w - 1, h - 1)) . toRobotMap . fromRight [] . A.parseOnly (parseRobot `A.sepBy1` A.endOfLine) <$>) . BSC.readFile . ("src/" ++)

printGrid :: Area -> String
printGrid a = intercalate "\n" ([concat [maybe "." (show . S.size) (M.lookup (x, y) (robots' a)) | x <- [minX .. maxX]] | y <- [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds' a

quadrants :: Area -> [Area]
quadrants a = map toArea [topLeft, topRight, bottomLeft, bottomRight]
  where
    b@(_, (maxX, maxY)) = bounds' a
    topLeft@(_, (tlMaxX, tlMaxY)) = ((0, 0), (((maxX + 1) `div` 2) - 1, ((maxY + 1) `div` 2) - 1))
    topRight = ((tlMaxX + 2, 0), (maxX, tlMaxY))
    bottomLeft = ((0, tlMaxY + 2), (tlMaxX, maxY))
    bottomRight = ((tlMaxX + 2, tlMaxY + 2), (maxX, maxY))
    toArea b@((minX, minY), (maxX, maxY)) = Area b $ M.filterWithKey (\(x, y) _ -> x >= minX && x <= maxX && y >= minY && y <= maxY) (robots' a)

safetyFactor :: Area -> Int
safetyFactor = product . map (M.foldl (+) 0 . M.map S.size . robots') . quadrants

-- >>>  part1 <$> loadInput (11, 7) "example.txt"
-- 12
part1 :: Area -> Int
part1 = safetyFactor . (`step` 100)

step :: Area -> Int -> Area
step a n = a {robots' = robots''}
  where
    (_, (maxX, maxY)) = bounds' a
    robots'' = toRobotMap $ concatMap (S.toList . S.map stepRobot) $ M.elems $ robots' a
    stepRobot r = let (vx, vy) = v' r in r {p' = bimap ((`mod` (maxX + 1)) . (+ vx * n)) ((`mod` (maxY + 1)) . (+ vy * n)) $ p' r}

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

mad :: Area -> Int
mad a = mean differences
  where
    differences = map (\(x, y) -> abs (mx - x) + abs (my - y)) $ M.keys $ robots' a
    (mx, my) = join bimap mean $ unzip $ M.keys $ robots' a

-- >>> leastMad <$> loadInput (101, 103) "input.txt"
-- (7492,..........................................1..........................................................
-- .....................................1.................1..........................1..................
-- .........................................1........1..................................................
-- .....................................................................................................
-- ....1............................................1.....................1.....................1.......
-- .....................................................................................................
-- ....................................................1...1............................................
-- ...........1......................1.......................1..................................1.......
-- ...............................................................1.....................................
-- ................................................................1....................................
-- .........................1.................1.........................................................
-- .....................................................................................................
-- ...........................1..................................1....1.............1................1..
-- ..1..................................................................................................
-- .....................................................................................................
-- ..........................................................................1.................1........
-- .....................................................................................................
-- .....................................1...............................................................
-- ...1..........................................1......................................................
-- ...................................................1...............1...........1.....................
-- .....................................................................................................
-- ..............1.......................................1..............................................
-- .....................................................................................................
-- .....................1..............................................1...................1............
-- ...................1.................................................................................
-- .............................................................................................1.......
-- ......1..............................................1................................1..............
-- ...1.............................11.1................................................................
-- ....1................................1...............................................................
-- .....................................................................................................
-- .....................................................................................................
-- ..............................................1......1...........................1...................
-- .............................................................................1.......................
-- .............................................................................................1.......
-- ........1............1................................................................1..............
-- .................................................................1...................................
-- ........................................1..................1.........................................
-- ......................................1...................................1.........1.........1..1...
-- .....................................................................................................
-- ........................1.........1..................................................................
-- ..1........................................................1.........................................
-- .................1111111111111111111111111111111............................1........................
-- ...............1.1.............................1..................................1..................
-- .................1.............................1..........................................1..........
-- .................1.............................1............................1........................
-- .................1.............................1.....................................................
-- .................1..............1..............1.....................................................
-- .................1.............111.............1........................1..1.........................
-- .................1............11111............1.....................................................
-- .................1...........1111111...........1.....................................................
-- .................1..........111111111..........1..............11....................1................
-- .................1............11111............1...................................1.................
-- .................1...........1111111...........1.....................1...............................
-- .................1..........111111111..........1.....................................................
-- .................1.........11111111111.........1....................1...............1................
-- .................1........1111111111111........1..............................1......................
-- .................1..........111111111..........1.....................................................
-- .....1...........1.........11111111111.........1.............................1.......................
-- .................1........1111111111111........1.....................................................
-- .................1.......111111111111111.......1.....................................................
-- ..........1......1......11111111111111111......1....................................1................
-- .................1........1111111111111........1.....................................................
-- .......1.........1.......111111111111111.......1.............1...........1...........................
-- .................1......11111111111111111......1..........1..........................................
-- .................1.....1111111111111111111.....1.....................................................
-- .................1....111111111111111111111....1.....................................................
-- .................1.............111.............1................................1....1...............
-- ..1.........1....1.............111.............1.........1.............1.............................
-- .................1.............111.............1........................1......................1.....
-- .................1.............................1......................................1..............
-- .................1.............................1..1.1........................1.....1.................
-- .................1.............................1.................................................1.1.
-- .................1.............................1.....................................................
-- .................1111111111111111111111111111111.....................................................
-- ............................................................................1........................
-- ..............................................................................................1......
-- ...............1..................................................................1..................
-- .......................................................................1.............................
-- ..1................................1...1...................................1.....1...................
-- .1..............1.1..................1...............................................................
-- .....................................................................................................
-- .....................................................................................................
-- ...................................1....1.........1........................................1.........
-- .....................................................................................................
-- 1....1..................1............................................................................
-- ..........1....1................1....................1...............................................
-- .....................................................................................................
-- .....................1...............................................................................
-- .....................................................................................................
-- .......................................................................................1.............
-- .....................................................................................................
-- ..............................................................1......................................
-- .....................................................................................................
-- ....................................................1..........1.....................................
-- .......................1.....................................................1.......................
-- .....................................................................................................
-- .....................................................................................................
-- ..........................................................................................1..........
-- .......................................1.............................................................
-- .....................................................................................................
-- .......1.............................................................................................
-- ........................................................................................1............
-- ........................................1.................................1................1.........)
leastMad :: Area -> (Integer, Area)
leastMad = minimumBy (comparing (mad . snd)) . zip [1 ..] . take 10000 . drop 1 . iterate (`step` 1)

-- >>> part2 <$> loadInput (101, 103) "input.txt"
-- 7492
part2 :: Area -> Integer
part2 = fst . leastMad
