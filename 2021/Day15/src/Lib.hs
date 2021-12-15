{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    exitCost,
    expandLandscape,
  )
where

import Control.Monad (ap)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import qualified Data.Set as S

type Coordinate = (Int, Int)

type Landscape = M.Map Coordinate Int

-- Point -> Cost to get to
type SolveState = M.Map Coordinate (Maybe Int)

-- >>> A.parseOnly parseRow "5483143223"
-- Right [5,4,8,3,1,4,3,2,2,3]
parseRow :: A.Parser [Int]
parseRow = map digitToInt <$> A.many1 A.digit

parseRows :: A.Parser [[Int]]
parseRows = A.sepBy1 parseRow A.endOfLine

-- >>> rowsToLandscape [[2,1,9,9,9,4,3,2,1,0],[3,9,8,7,8,9,4,9,2,1],[9,8,5,6,7,8,9,8,9,2],[8,7,6,7,8,9,6,7,8,9],[9,8,9,9,9,6,5,6,7,8]]
-- fromList [((0,0),9),((0,1),8),((0,2),9),((0,3),3),((0,4),2),((1,0),8),((1,1),7),((1,2),8),((1,3),9),((1,4),1),((2,0),9),((2,1),6),((2,2),5),((2,3),8),((2,4),9),((3,0),9),((3,1),7),((3,2),6),((3,3),7),((3,4),9),((4,0),9),((4,1),8),((4,2),7),((4,3),8),((4,4),9),((5,0),6),((5,1),9),((5,2),8),((5,3),9),((5,4),4),((6,0),5),((6,1),6),((6,2),9),((6,3),4),((6,4),3),((7,0),6),((7,1),7),((7,2),8),((7,3),9),((7,4),2),((8,0),7),((8,1),8),((8,2),9),((8,3),2),((8,4),1),((9,0),8),((9,1),9),((9,2),2),((9,3),1),((9,4),0)]
rowsToLandscape :: [[Int]] -> Landscape
rowsToLandscape rows = M.fromList $ concatMap reorderWithCoordinate $ zip ys $ map (zip [0 ..]) rows
  where
    reorderWithCoordinate (y, row) = map (\(x, val) -> ((x, y), val)) row
    ys = [0 .. height -1]
    height = length rows

adjacents :: Landscape -> Coordinate -> [Coordinate]
adjacents landscape (x, y) = filter (\c -> S.member c (M.keysSet landscape)) [(x - 1, y), (x + 1, y), (x, y -1), (x, y + 1)]

-- >>> loadInput "example.txt"
-- fromList [((0,0),2),((0,1),1),((0,2),3),((0,3),1),((0,4),1),((0,5),7),((0,6),3),((0,7),2),((0,8),1),((0,9),1),((1,0),3),((1,1),2),((1,2),1),((1,3),3),((1,4),3),((1,5),4),((1,6),6),((1,7),1),((1,8),3),((1,9),1),((2,0),1),((2,1),9),((2,2),2),((2,3),5),((2,4),1),((2,5),6),((2,6),9),((2,7),3),((2,8),8),((2,9),6),((3,0),1),((3,1),3),((3,2),5),((3,3),9),((3,4),9),((3,5),3),((3,6),4),((3,7),6),((3,8),1),((3,9),3),((4,0),9),((4,1),1),((4,2),4),((4,3),9),((4,4),1),((4,5),4),((4,6),9),((4,7),5),((4,8),3),((4,9),7),((5,0),4),((5,1),3),((5,2),2),((5,3),1),((5,4),2),((5,5),1),((5,6),3),((5,7),1),((5,8),7),((5,9),5),((6,0),4),((6,1),8),((6,2),1),((6,3),2),((6,4),8),((6,5),7),((6,6),1),((6,7),1),((6,8),3),((6,9),1),((7,0),5),((7,1),5),((7,2),6),((7,3),4),((7,4),1),((7,5),1),((7,6),5),((7,7),3),((7,8),6),((7,9),7),((8,0),8),((8,1),2),((8,2),3),((8,3),2),((8,4),3),((8,5),1),((8,6),6),((8,7),2),((8,8),7),((8,9),4),((9,0),1),((9,1),1),((9,2),9),((9,3),1),((9,4),7),((9,5),1),((9,6),9),((9,7),8),((9,8),2),((9,9),2)]
loadInput :: [Char] -> IO Landscape
loadInput fileName =
  rowsToLandscape . fromRight []
    . A.parseOnly parseRows
    <$> BSC.readFile
      ("src/" ++ fileName)

-- >>> floodFill <$> loadInput "example.txt"
-- fromList [((0,0),Just 0),((0,1),Just 1),((0,2),Just 3),((0,3),Just 6),((0,4),Just 13),((0,5),Just 14),((0,6),Just 15),((0,7),Just 18),((0,8),Just 19),((0,9),Just 21),((1,0),Just 1),((1,1),Just 4),((1,2),Just 4),((1,3),Just 10),((1,4),Just 14),((1,5),Just 17),((1,6),Just 18),((1,7),Just 19),((1,8),Just 21),((1,9),Just 24),((2,0),Just 7),((2,1),Just 12),((2,2),Just 7),((2,3),Just 16),((2,4),Just 20),((2,5),Just 18),((2,6),Just 23),((2,7),Just 21),((2,8),Just 30),((2,9),Just 25),((3,0),Just 10),((3,1),Just 11),((3,2),Just 13),((3,3),Just 17),((3,4),Just 20),((3,5),Just 27),((3,6),Just 32),((3,7),Just 26),((3,8),Just 29),((3,9),Just 26),((4,0),Just 17),((4,1),Just 14),((4,2),Just 18),((4,3),Just 26),((4,4),Just 24),((4,5),Just 25),((4,6),Just 34),((4,7),Just 30),((4,8),Just 30),((4,9),Just 35),((5,0),Just 22),((5,1),Just 21),((5,2),Just 19),((5,3),Just 22),((5,4),Just 23),((5,5),Just 25),((5,6),Just 26),((5,7),Just 28),((5,8),Just 31),((5,9),Just 35),((6,0),Just 23),((6,1),Just 23),((6,2),Just 20),((6,3),Just 21),((6,4),Just 28),((6,5),Just 33),((6,6),Just 28),((6,7),Just 29),((6,8),Just 37),((6,9),Just 39),((7,0),Just 30),((7,1),Just 29),((7,2),Just 23),((7,3),Just 26),((7,4),Just 27),((7,5),Just 28),((7,6),Just 32),((7,7),Just 35),((7,8),Just 40),((7,9),Just 44),((8,0),Just 34),((8,1),Just 32),((8,2),Just 25),((8,3),Just 31),((8,4),Just 28),((8,5),Just 31),((8,6),Just 33),((8,7),Just 36),((8,8),Just 38),((8,9),Just 46),((9,0),Just 36),((9,1),Just 34),((9,2),Just 33),((9,3),Just 38),((9,4),Just 29),((9,5),Just 36),((9,6),Just 34),((9,7),Just 43),((9,8),Just 39),((9,9),Just 40)]
floodFill :: Landscape -> SolveState
floodFill landscape = floodRec initialState [(0, 0)]
  where
    adjacentMap = M.fromList $ map (\point -> (point, S.fromList $ adjacents landscape point)) $ M.keys landscape
    initialState = M.insert (0, 0) (Just 0) $ M.map (const Nothing) landscape
    floodRec :: SolveState -> [Coordinate] -> SolveState
    floodRec state [] = state
    floodRec state previouslyUpdated = floodRec (M.union (M.fromList mapUpdates) state) $ map fst mapUpdates
      where
        potentiallyRecalculated = S.toList $ S.unions $ map (fromJust . (`M.lookup` adjacentMap)) previouslyUpdated
        mapUpdates = filter (\(point, newValue) -> newValue /= fromJust (M.lookup point state)) $ map (ap (,) newValue) potentiallyRecalculated
        newValue point = case currentValue of
          Just value -> Just $ minimum (value : options)
          _ -> Just $ minimum options
          where
            Just enterCost = M.lookup point landscape
            options = map (+ enterCost) $ catMaybes $ S.toList $ S.map (fromJust . (`M.lookup` state)) $ fromJust $ M.lookup point adjacentMap
            Just currentValue = M.lookup point state

-- >>> exitCost <$> loadInput "example.txt"
-- 40
exitCost :: Landscape -> Int
exitCost landscape = fromJust $ fromJust $ M.lookup (maxX, maxY) solved
  where
    solved = floodFill landscape
    maxX = maximum $ map fst $ M.keys solved
    maxY = maximum $ map snd $ M.keys solved

-- >>> exitCost <$> loadInput "input.txt"
-- 447

expandLandscape :: Landscape -> Landscape
expandLandscape landscape = M.fromList $ concat [map (\((x, y), value) -> ((x + (xOffset * width), y + (yOffset * height)), roundCorrectly (value + xOffset + yOffset))) (M.toList landscape) | xOffset <- [0 .. 4], yOffset <- [0 .. 4]]
  where
    roundCorrectly val = cycle [1 .. 9] !! (val - 1)
    width = 1 + maximum (map fst $ M.keys landscape)
    height = 1 + maximum (map snd $ M.keys landscape)

-- >>> wrongExpansions
-- []
wrongExpansions = do
  example <- loadInput "example.txt"
  let expandedAttempt = expandLandscape example
  truth <- loadInput "exampleExpanded.txt"
  return $ filter (\((_, v1), (_, v2)) -> v1 /= v2) $ zip (M.toList expandedAttempt) (M.toList truth)

-- >>> exitCost . expandLandscape <$> loadInput "example.txt"
-- 315
