{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (tails)
import Data.Maybe (mapMaybe)

type Vec3D = (Int, Int, Int)

data HailStone = HailStone {name :: String, position :: Vec3D, velocity :: Vec3D} deriving (Show, Eq, Ord)

data Dimension = X | Y | Z deriving (Show, Eq, Ord)

data CollisionPossibility = AlwaysCollide | NeverCollide | CollideAt Double | Intersect [CollisionForDimension] deriving (Show, Eq, Ord)

data CollisionForDimension = CollisionForDimension {dimension :: Dimension, possibility :: CollisionPossibility} deriving (Show, Eq, Ord)

-- >>> A.parseOnly parseVec3D "-2,  1, -2"
-- Right (-2,1,-2)
parseVec3D :: A.Parser Vec3D
parseVec3D = (\[x, y, z] -> (x, y, z)) <$> A.signed A.decimal `A.sepBy1` ("," *> A.skipSpace)

-- >>> A.parseOnly parseHailStone "19, 13, 30 @ -2,  1, -2"
-- Right (HailStone {name = "", position = (19,13,30), velocity = (-2,1,-2)})
parseHailStone :: A.Parser HailStone
parseHailStone = HailStone "" <$> (parseVec3D <* (A.skipSpace *> "@" <* A.skipSpace)) <*> parseVec3D

parseHailStones :: A.Parser [HailStone]
parseHailStones = zipWith (\c h -> h {name = pure c}) ['A' ..] <$> parseHailStone `A.sepBy1` A.endOfLine

-- >>> loadInput "example.txt"
-- [HailStone {name = "A", position = (19,13,30), velocity = (-2,1,-2)},HailStone {name = "B", position = (18,19,22), velocity = (-1,-1,-2)},HailStone {name = "C", position = (20,25,34), velocity = (-2,-2,-4)},HailStone {name = "D", position = (12,31,28), velocity = (-1,-2,-1)},HailStone {name = "E", position = (20,19,15), velocity = (1,-5,-3)}]
loadInput :: [Char] -> IO [HailStone]
loadInput = (fromRight [] . A.parseOnly parseHailStones <$>) . BSC.readFile . ("src/" ++)

-- >>> pairs [1..5]
-- [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]
pairs :: [a] -> [(a, a)]
pairs l = [(a, b) | (a : bs) <- tails l, b <- bs]

-- >>> map lineLineIntersectionXY . pairs <$> loadInput "example.txt"
-- [Just (14.333333333333332,15.333333333333334),Just (11.666666666666668,16.666666666666668),Just (6.199999999999999,19.4),Nothing,Nothing,Just (-6.0,-5.0),Nothing,Just (-2.0,3.0),Nothing,Nothing]

-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
lineLineIntersectionXY :: (HailStone, HailStone) -> Maybe (Double, Double)
lineLineIntersectionXY (HailStone _ (px, py, _) (vx, vy, _), HailStone _ (px', py', _) (vx', vy', _)) =
  let dx = px - px'
      dy = py - py'
      den = (fromIntegral $ vy * vx' - vx * vy')
   in if den == 0
        then Nothing
        else
          let t = (fromIntegral (vy' * dx - vx' * dy) / den)
              t' = (fromIntegral (vy * dx - vx * dy) / den)
           in if t > 0 && t' > 0
                then Just (fromIntegral px + fromIntegral vx * t, fromIntegral py + fromIntegral vy * t)
                else Nothing

-- >>> part1 (7, 27) <$> loadInput "example.txt"
-- 2
part1 :: (Int, Int) -> [HailStone] -> Int
part1 (lo, hi) = length . filter (\(x, y) -> inbound x && inbound y) . mapMaybe lineLineIntersectionXY . pairs
  where
    inbound x = x >= fromIntegral lo && x <= fromIntegral hi
