{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    solve,
    countVisible,
    step,
    bestScore,
  )
where

import Control.Lens (each, over)
import Control.Monad (ap)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Ix (Ix (inRange))
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
import Data.Set qualified as S
import GHC.Base ((<|>))

type CoordinateMap = M.Map (Int, Int) Int

type BoundedCoordinateMap = ((Int, Int), CoordinateMap)

type LocationPerHeight = Maybe (M.Map Int (Int, Int))

data HeightKnowledge = HeightKnowledge {fromNorth :: LocationPerHeight, fromEast :: LocationPerHeight, fromSouth :: LocationPerHeight, fromWest :: LocationPerHeight}
  deriving (Eq, Show)

type HeightKnowledgeMap = M.Map (Int, Int) HeightKnowledge

type State = (BoundedCoordinateMap, HeightKnowledgeMap, S.Set (Int, Int))

unknownVis :: HeightKnowledge
unknownVis = HeightKnowledge Nothing Nothing Nothing Nothing

-- >>> toCoordinateMap [[3,0,3,7,3],[2,5,5,1,2]]
-- ((4,1),fromList [((0,0),3),((0,1),2),((1,0),0),((1,1),5),((2,0),3),((2,1),5),((3,0),7),((3,1),1),((4,0),3),((4,1),2)])
toCoordinateMap :: [[Int]] -> BoundedCoordinateMap
toCoordinateMap values = ((xMax, yMax),) $ M.fromList $ concat annotatedValues
  where
    xMax = length (head annotatedValues) - 1
    yMax = length annotatedValues - 1
    annotatedValues = zipWith (\y -> map (\(x, v) -> ((x, y), v))) [0 ..] $ map (zip [0 ..]) values

-- >>> A.parseOnly parseLine "25512"
-- Right [2,5,5,1,2]
parseLine :: A.Parser [Int]
parseLine = A.many1 (read . return <$> A.digit)

-- >>> A.parseOnly parseGrid "30373\n25512"
-- Right ((4,1),fromList [((0,0),3),((0,1),2),((1,0),0),((1,1),5),((2,0),3),((2,1),5),((3,0),7),((3,1),1),((4,0),3),((4,1),2)])
parseGrid :: A.Parser BoundedCoordinateMap
parseGrid = toCoordinateMap <$> A.sepBy1 parseLine A.endOfLine

mergePartialKnowledge :: HeightKnowledge -> HeightKnowledge -> HeightKnowledge
mergePartialKnowledge (HeightKnowledge n e s w) (HeightKnowledge n' e' s' w') = HeightKnowledge (n <|> n') (e <|> e') (s <|> s') (w <|> w')

toInitialState :: BoundedCoordinateMap -> State
toInitialState bCMap@(limits@(xMax, yMax), cMap) =
  ( bCMap,
    vMap,
    S.unions $ map (\((x, y), v) -> frontierDelta (x, y) limits v unknownVis) $ M.toList startingPerimeter
  )
  where
    vMap = M.union startingPerimeter $ M.map (pure unknownVis) cMap
    startingPerimeter = M.unionsWith mergePartialKnowledge [topEdge, rightEdge, bottomEdge, leftEdge]
    topEdge = M.fromList $ map (\x -> ((x, 0), unknownVis {fromNorth = Just M.empty})) [0 .. xMax]
    bottomEdge = M.fromList $ map (\x -> ((x, yMax), unknownVis {fromSouth = Just M.empty})) [0 .. xMax]
    leftEdge = M.fromList $ map (\y -> ((0, y), unknownVis {fromWest = Just M.empty})) [0 .. yMax]
    rightEdge = M.fromList $ map (\y -> ((xMax, y), unknownVis {fromEast = Just M.empty})) [0 .. yMax]

frontierDelta :: (Int, Int) -> (Int, Int) -> HeightKnowledge -> HeightKnowledge -> S.Set (Int, Int)
frontierDelta (x, y) (xMax, yMax) v' v =
  S.fromList $
    filter (\(x', y') -> inRange (0, xMax) x' && inRange (0, yMax) y') $
      catMaybes
        [ if isJust (fromNorth v') && isNothing (fromNorth v) then Just (x, y + 1) else Nothing,
          if isJust (fromEast v') && isNothing (fromEast v) then Just (x - 1, y) else Nothing,
          if isJust (fromSouth v') && isNothing (fromSouth v) then Just (x, y - 1) else Nothing,
          if isJust (fromWest v') && isNothing (fromWest v) then Just (x + 1, y) else Nothing
        ]

loadInput :: String -> IO State
loadInput = (toInitialState . (fromRight ((0, 0), M.empty) . A.parseOnly parseGrid) <$>) . BSC.readFile . ("src/" ++)

step :: State -> State
step (bCMap@(limits, cMap), kMap, frontier) = (bCMap, kMap', frontier')
  where
    kMap' = M.union (M.fromList kMapDelta) kMap
    frontier' = S.unions $ map (\(p@(x, y), v) -> frontierDelta (x, y) limits v (fromJust $ M.lookup p kMap)) kMapDelta
    kMapDelta = map (ap (,) knowledge) $ S.toList frontier
    knowledge p@(x, y) =
      mergePartialKnowledge
        (fromJust $ M.lookup p kMap)
        ( HeightKnowledge
            { fromNorth = calculateDirection (x, y - 1) fromNorth,
              fromEast = calculateDirection (x + 1, y) fromEast,
              fromSouth = calculateDirection (x, y + 1) fromSouth,
              fromWest = calculateDirection (x - 1, y) fromWest
            }
        )
      where
        calculateDirection point dir = fmap (M.insert (1 + fromJust (M.lookup point cMap)) point) (dir =<< M.lookup point kMap)

solve :: State -> State
solve input
  | next /= input = solve next
  | otherwise = input
  where
    next = step input

countVisible :: State -> Int
countVisible ((_, cMap), vMap, _) = length $ filter (uncurry isVisible) $ zip (M.elems cMap) (M.elems vMap)

isVisible :: Int -> HeightKnowledge -> Bool
isVisible height (HeightKnowledge n e s w) = or $ mapMaybe (fmap ((height >=) . maximum . (0 :) . M.keys)) [n, e, s, w]

scenicScore :: HeightKnowledgeMap -> BoundedCoordinateMap -> (Int, Int) -> Int
scenicScore kMap ((xMax, yMax), cMap) p@(x, y) = product [northScore, eastScore, southScore, westScore]
  where
    northScore = (minimum . (y :) . map snd) northScores
    eastScore = (minimum . ((xMax - x) :) . map snd) eastScores
    southScore = (minimum . (yMax - y :) . map snd) southScores
    westScore = (minimum . (x :) . map snd) westScores
    (northScores, eastScores, southScores, westScores) = over each (filter ((> height) . fst) . M.toList) (northDistances, eastDistances, southDistances, westDistances)
    (eastDistances, westDistances) = over each (M.map (\(x', _) -> abs (x - x')) . fromJust) (e, w)
    (northDistances, southDistances) = over each (M.map (\(_, y') -> abs (y - y')) . fromJust) (n, s)
    height = fromJust $ M.lookup p cMap
    HeightKnowledge n e s w = fromJust $ M.lookup p kMap

bestScore :: State -> Int
bestScore (bCMap, kMap, _) = maximum $ map (scenicScore kMap bCMap) $ M.keys kMap
