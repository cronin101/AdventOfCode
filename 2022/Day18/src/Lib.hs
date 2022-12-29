{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    surfaceArea,
    boundingBox,
    exteriorSurfaceArea,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Ix (inRange)
import Data.Set qualified as S

type Coord3D = (Int, Int, Int)

-- >>> A.parseOnly parseCoord3D "1,2,3"
-- Right (1,2,3)
parseCoord3D :: A.Parser Coord3D
parseCoord3D = (,,) <$> A.signed A.decimal <* "," <*> A.signed A.decimal <* "," <*> A.signed A.decimal

parseCoords :: A.Parser [Coord3D]
parseCoords = A.sepBy1 parseCoord3D A.endOfLine

loadInput :: String -> IO [Coord3D]
loadInput = (fromRight [] . A.parseOnly parseCoords <$>) . BSC.readFile . ("src/" ++)

-- >>> adjacents (1, 1, 1)
-- [(0,1,1),(1,0,1),(1,1,0),(1,1,2),(1,2,1),(2,1,1)]
adjacents :: Coord3D -> [Coord3D]
adjacents (x, y, z) =
  [ (x + dx, y + dy, z + dz)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      dz <- [-1 .. 1],
      (== 2) $ length $ filter (== 0) [dx, dy, dz]
  ]

surfaceArea :: [Coord3D] -> Int
surfaceArea coords = sum exposedSides
  where
    exposedSides = map (S.size . flip S.difference coordSet . S.fromList . adjacents) coords
    coordSet = S.fromList coords

exteriorSurfaceArea :: [Coord3D] -> Int
exteriorSurfaceArea coords = sum exposedSides
  where
    troposphere = findTroposphere coords
    exposedSides = map (S.size . S.intersection troposphere . S.fromList . adjacents) $ S.toList coordSet
    coordSet = S.fromList coords

boundingBox :: [Coord3D] -> (Coord3D, Coord3D)
boundingBox coords = ((S.findMin xs - 1, S.findMin ys - 1, S.findMin zs - 1), (S.findMax xs + 1, S.findMax ys + 1, S.findMax zs + 1))
  where
    toSingletonMaps (x, y, z) = (S.singleton x, S.singleton y, S.singleton z)
    (xs, ys, zs) = foldl1 (\(xs', ys', zs') (xs'', ys'', zs'') -> (S.union xs' xs'', S.union ys' ys'', S.union zs' zs'')) $ map toSingletonMaps coords

findTroposphere :: [Coord3D] -> S.Set Coord3D
findTroposphere coords = floodFill (S.singleton $ fst bb) S.empty S.empty
  where
    bb@((xMin, yMin, zMin), (xMax, yMax, zMax)) = boundingBox coords
    coordSet = S.fromList coords
    floodFill frontier visited troposphere
      | S.null frontier = troposphere
      | otherwise = floodFill frontier' visited' troposphere'
      where
        troposphere' = troposphere `S.union` S.filter (any (`S.member` coordSet) . adjacents) frontier
        visited' = visited `S.union` frontier
        isInRange (x, y, z) = inRange (xMin, xMax) x && inRange (yMin, yMax) y && inRange (zMin, zMax) z
        validFrontier f = not (S.member f coordSet) && not (S.member f visited)
        adjacentsToFrontier = S.filter isInRange $ S.fromList $ concatMap adjacents $ S.toList frontier
        frontier' = S.filter validFrontier adjacentsToFrontier
