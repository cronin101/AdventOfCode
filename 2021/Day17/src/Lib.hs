{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    findMaxHeight,
    validVelocities,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit)
import qualified Data.Char as A
import Data.Either (fromRight, isRight)
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt)

-- ((xMin, xMax), (yMin, yMax))
type Target = ((Int, Int), (Int, Int))

-- >>> A.parseOnly parseTarget "target area: x=20..30, y=-10..-5"
-- Right ((20,30),(-10,-5))
parseTarget :: A.Parser Target
parseTarget = do
  "target area: x="
  xMin <- signedInt
  ".."
  xMax <- signedInt
  ", y="
  yMin <- signedInt
  ".."
  yMax <- signedInt
  return ((xMin, xMax), (yMin, yMax))
  where
    signedInt = read . BSC.unpack <$> A.takeWhile1 (\c -> A.isNumber c || (c == '-'))

-- >>> loadInput "example.txt"
-- ((20,30),(-10,-5))
loadInput :: [Char] -> IO ((Int, Int), (Int, Int))
loadInput fileName =
  fromRight ((0, 0), (0, 0))
    . A.parseOnly parseTarget
    <$> BSC.readFile
      ("src/" ++ fileName)

futures :: (Int, Int) -> [(Int, Int)]
futures velocity@(vx, vy) = map fst $ iterate (\((rx, ry), (vx, vy)) -> ((rx + vx, ry + vy), (if vx > 0 then vx - 1 else 0, vy - 1))) ((0, 0), velocity)

futureYs = map snd . futures . (0,)

findMaxVY = last . validPositiveYVelocities

validNegativeYVelocities :: (Int, Int) -> [Int]
validNegativeYVelocities target@(tmin, tmax) = filter (any (\y -> y <= tmax && y >= tmin) . takeWhile (>= tmin) . futureYs) [tmin .. (-1)]

validPositiveYVelocities :: (Int, Int) -> [Int]
validPositiveYVelocities target@(tmin, tmax) = map fst $ filter ((<= tmax) . last . snd) $ takeWhile ((>= 2) . length . snd) velocitiesWithFutures
  where
    velocitiesWithFutures :: [(Int, [Int])]
    velocitiesWithFutures = map (\x -> (x, dropWhile (> 0) . tail . takeWhile (>= tmin) $ futureYs x)) [0 ..]

-- >>>  findMaxHeight . snd <$> loadInput "example.txt"
-- 45
findMaxHeight target@(tmin, tmax) = fst $ head $ dropWhile (\(c, n) -> n >= c) $ zip (futureYs vy) (drop 1 $ futureYs vy)
  where
    vy = findMaxVY target

-- >>> length . validVelocities <$> loadInput "example.txt"
-- 112
validVelocities target@(xTarget@(xmin, xmax), yTarget@(ymin, ymax)) = filter (any (\(x, y) -> x <= xmax && x >= xmin && y <= ymax && y >= ymin) . takeWhile ((>= ymin) . snd) . futures) candidates
  where
    candidates = concatMap (flip zip validYVelocities . repeat) [1 .. xmax]
    validYVelocities = validNegativeYVelocities yTarget ++ validPositiveYVelocities yTarget
