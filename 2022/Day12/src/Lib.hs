{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    initialState,
    solutionA,
    solutionB,
  )
where

import Control.Monad (ap)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlpha, ord)
import Data.Either (fromRight)
import Data.Ix (Ix (inRange))
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)

type Coordinate = (Int, Int)

type HeightMap = M.Map Coordinate Cell

type BoundedHeightMap = (Coordinate, HeightMap)

type Cell = (Char, Int)

-- ((start, end), heightMap)
type Input = ((Coordinate, Coordinate), BoundedHeightMap)

-- (length remaining, next coordinate)
type DirectionsMap = M.Map Coordinate (Int, Coordinate)

parseCell :: A.Parser Cell
parseCell = do
  c <- A.satisfy isAlpha
  let height = case c of
        'S' -> 0
        'E' -> 25
        _ -> ord c - 97
  return (c, height)

-- >>> A.parseOnly parseLine "SabqponzE"
-- Right [('S',0),('a',0),('b',1),('q',16),('p',15),('o',14),('n',13),('z',25),('E',26)]
parseLine :: A.Parser [Cell]
parseLine = A.many1 parseCell

-- >>> A.parseOnly parseGrid "Sabqpon\nabcryxxl"
-- Right ((6,1),fromList [((0,0),('S',0)),((0,1),('a',0)),((1,0),('a',0)),((1,1),('b',1)),((2,0),('b',1)),((2,1),('c',2)),((3,0),('q',16)),((3,1),('r',17)),((4,0),('p',15)),((4,1),('y',24)),((5,0),('o',14)),((5,1),('x',23)),((6,0),('n',13)),((6,1),('x',23)),((7,1),('l',11))])
parseGrid :: A.Parser BoundedHeightMap
parseGrid = toCoordinateMap <$> A.sepBy1 parseLine A.endOfLine

toCoordinateMap :: [[Cell]] -> BoundedHeightMap
toCoordinateMap values = ((xMax, yMax),) $ M.fromList $ concat annotatedValues
  where
    xMax = length (head annotatedValues) - 1
    yMax = length annotatedValues - 1
    annotatedValues = zipWith (\y -> map (\(x, v) -> ((x, y), v))) [0 ..] $ map (zip [0 ..]) values

loadInput :: String -> IO Input
loadInput filename = withEndpoints . fromRight ((0, 0), M.empty) . A.parseOnly parseGrid <$> BSC.readFile ("src/" ++ filename)
  where
    withEndpoints :: BoundedHeightMap -> Input
    withEndpoints bMap@(_, m) =
      let findChar char = fst $ fromJust $ find ((== char) . fst . snd) $ M.toList m
          start = findChar 'S'
          end = findChar 'E'
       in ((start, end), bMap)

neighbours :: BoundedHeightMap -> Coordinate -> [Coordinate]
neighbours ((xMax, yMax), cellMap) p@(x, y) = filter notTooSteep $ filter inbounds candidates
  where
    candidates = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
    notTooSteep p' = snd (cellMap M.! p) - snd (cellMap M.! p') <= 1
    inbounds (x', y') = inRange (0, xMax) x' && inRange (0, yMax) y'

initialState :: Input -> DirectionsMap
initialState ((_, end), bcm) = M.fromList $ map (,(1, end)) $ neighbours bcm end

step :: Input -> ([Coordinate], DirectionsMap) -> ([Coordinate], DirectionsMap)
step (_, bcm) (frontier, dir) = (frontier', dir')
  where
    frontier' = filter (\f -> M.lookup f dir' /= M.lookup f dir) $ M.keys dir'
    dir' = M.unionWith takeCheapest dir $ M.unionsWith takeCheapest newDirections
    takeCheapest e@(l, _) e'@(l', _) = if l' < l then e' else e
    newDirections = map (directionsFromValidNeighbours . ap (,) (dir M.!)) frontier
    directionsFromValidNeighbours (point, (len, _)) = M.fromList $ map (,(len + 1, point)) $ neighbours bcm point

convergenceValue :: Eq a => [a] -> a
convergenceValue (x : y : zs) = if x == y then x else convergenceValue (y : zs)
convergenceValue _ = error "No fixed value"

solve :: Input -> DirectionsMap
solve input = snd $ convergenceValue $ iterate (step input) startState
  where
    startState = (M.keys $ initialState input, initialState input)

solutionA :: Input -> Int
solutionA input@((start, _), _) = fst $ fromJust $ M.lookup start (solve input)

solutionB :: Input -> Int
solutionB input@(_, (_, cmap)) = minimum $ mapMaybe (\a -> fst <$> M.lookup a solved) as
  where
    as = M.keys $ M.filter ((== 'a') . fst) cmap
    solved = solve input
