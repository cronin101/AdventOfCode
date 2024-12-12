{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
  )
where

import Control.Arrow (Arrow (second), first)
import Control.Monad (join)
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Data.Tuple (swap)

type Coord2D = (Int, Int)

type PlantForCoord = M.Map Coord2D Char

type RegionForCoord = M.Map Coord2D (S.Set Coord2D)

type CoordsForPlant = M.Map Char (S.Set Coord2D)

type MatchingNeighbours = M.Map Coord2D (S.Set Coord2D)

data Garden where
  Garden ::
    { -- plantForCoord :: PlantForCoord,
      regionForCoord :: RegionForCoord,
      -- coordsForPlant :: CoordsForPlant,
      matchingNeighbours :: MatchingNeighbours
    } ->
    Garden
  deriving (Show, Eq)

-- >>> A.parseOnly parseRow "AABBC"
-- Right [((0,0),'A'),((1,0),'A'),((2,0),'B'),((3,0),'B'),((4,0),'C')]
parseRow :: A.Parser [(Coord2D, Char)]
parseRow = zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (A.notChar '\n')

parseMap :: A.Parser Garden
parseMap = toGarden . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    toGarden m =
      let fracturedRegions = M.mapWithKey (const . S.singleton) m
          coordsForPlant' = M.fromListWith S.union $ map (second S.singleton . swap) $ M.toList m
          matchingNeighbours' = M.mapWithKey (\c@(x, y) _ -> S.intersection (S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]) (coordsForPlant' M.! (m M.! c))) m
       in mergeRegions $ Garden fracturedRegions matchingNeighbours'

growRegionsStep :: Garden -> Garden
growRegionsStep g@(Garden regions matchingNeighbours) =
  let neighbourSet = S.map (matchingNeighbours M.!)
      expandedRegions = M.map (\r -> S.unions (S.insert r $ neighbourSet r)) regions
      normalisedRegions = M.fromListWith S.union $ map (\(_, r) -> (S.findMin r, r)) $ M.toList expandedRegions
   in g {regionForCoord = normalisedRegions}

mergeRegions :: Garden -> Garden
mergeRegions g = denormaliseRegions $ fst $ until (uncurry (==)) (join bimap growRegionsStep) (growRegionsStep g, g)
  where
    denormaliseRegions g@(Garden regions _) = g {regionForCoord = M.fromList $ concatMap (\(_, r) -> map (,r) $ S.toList r) $ M.toList regions}

perimeter :: Garden -> Coord2D -> Int
perimeter g c = 4 - length (matchingNeighbours g M.! c)

-- >>> part1 <$> loadInput "example.txt"
-- 1930
part1 :: Garden -> Int
part1 g = sum $ map (\r -> length r * S.foldl (\r c -> r + perimeter g c) 0 r) $ nub $ M.elems $ regionForCoord g

loadInput :: [Char] -> IO Garden
loadInput = (fromRight (Garden M.empty M.empty) . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)
