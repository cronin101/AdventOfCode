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

-- The Historian needs to know the exact position and velocity of each robot.
-- This function parses the robot's position and velocity from the input string.
-- >>> A.parseOnly parseRobot "p=0,4 v=3,-3"
-- Right (Robot {p' = (0,4), v' = (3,-3)})
parseRobot :: A.Parser Robot
parseRobot = Robot <$> ("p=" *> parseCoord2D <* " v=") <*> parseCoord2D
  where
    parseCoord2D = (,) <$> (A.signed A.decimal <* ",") <*> A.signed A.decimal

-- To keep track of all robots, we need to map their positions to the robots themselves.
-- This function converts a list of robots into a map where the keys are positions and the values are sets of robots.
toRobotMap :: [Robot] -> M.Map Coord2D (S.Set Robot)
toRobotMap = M.fromListWith S.union . map (\r -> (p' r, S.singleton r))

-- The Historian needs to load the initial state of the robots from a file.
-- This function reads the file and parses the robots' positions and velocities.
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

-- To visualize the current state of the robots, we need to print the grid.
-- This function generates a string representation of the grid with the number of robots on each tile.
printGrid :: Area -> String
printGrid a = intercalate "\n" ([concat [maybe "." (show . S.size) (M.lookup (x, y) (robots' a)) | x <- [minX .. maxX]] | y <- [minY .. maxY]])
  where
    ((minX, minY), (maxX, maxY)) = bounds' a

-- To determine the safest area, we need to divide the grid into quadrants.
-- This function splits the area into four quadrants and filters the robots accordingly.
quadrants :: Area -> [Area]
quadrants a = map toArea [topLeft, topRight, bottomLeft, bottomRight]
  where
    b@(_, (maxX, maxY)) = bounds' a
    topLeft@(_, (tlMaxX, tlMaxY)) = ((0, 0), (((maxX + 1) `div` 2) - 1, ((maxY + 1) `div` 2) - 1))
    topRight = ((tlMaxX + 2, 0), (maxX, tlMaxY))
    bottomLeft = ((0, tlMaxY + 2), (tlMaxX, maxY))
    bottomRight = ((tlMaxX + 2, tlMaxY + 2), (maxX, maxY))
    toArea b@((minX, minY), (maxX, maxY)) = Area b $ M.filterWithKey (\(x, y) _ -> x >= minX && x <= maxX && y >= minY && y <= maxY) (robots' a)

-- To calculate the safety factor, we need to count the number of robots in each quadrant.
-- This function computes the product of the number of robots in each quadrant.
safetyFactor :: Area -> Int
safetyFactor = product . map (M.foldl (+) 0 . M.map S.size . robots') . quadrants

-- The Historian needs to know the safety factor after 100 seconds.
-- This function calculates the safety factor after simulating the robots' movements for 100 seconds.
-- >>>  part1 <$> loadInput (11, 7) "example.txt"
-- 12
part1 :: Area -> Int
part1 = safetyFactor . (`step` 100)

-- To predict the future positions of the robots, we need to simulate their movements.
-- This function updates the positions of the robots after a given number of seconds.
step :: Area -> Int -> Area
step a n = a {robots' = robots''}
  where
    (_, (maxX, maxY)) = bounds' a
    robots'' = toRobotMap $ concatMap (S.toList . S.map stepRobot) $ M.elems $ robots' a
    stepRobot r = let (vx, vy) = v' r in r {p' = bimap ((`mod` (maxX + 1)) . (+ vx * n)) ((`mod` (maxY + 1)) . (+ vy * n)) $ p' r}

-- To calculate the mean of a list of integers.
-- This function computes the average value of the list.
mean :: [Int] -> Int
mean xs = sum xs `div` length xs

-- To determine the mean absolute deviation, we need to calculate the average distance of the robots from the mean position.
-- This function computes the mean absolute deviation of the robots' positions.
meanAbsoluteDeviation :: Area -> Int
meanAbsoluteDeviation a = mean differences
  where
    differences = map (\(x, y) -> abs (mx - x) + abs (my - y)) $ M.keys $ robots' a
    (mx, my) = join bimap mean $ unzip $ M.keys $ robots' a

-- The Historian needs to find the fewest number of seconds for the robots to display the Easter egg.
-- This function finds the minimum mean absolute deviation over a series of steps.
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
leastMad = minimumBy (comparing (meanAbsoluteDeviation . snd)) . zip [1 ..] . take 10000 . drop 1 . iterate (`step` 1)

-- The Historian needs to know the fewest number of seconds for the robots to display the Easter egg.
-- This function calculates the number of seconds required for the robots to form the Easter egg pattern.
-- >>> part2 <$> loadInput (101, 103) "input.txt"
-- 7492
part2 :: Area -> Integer
part2 = fst . leastMad
