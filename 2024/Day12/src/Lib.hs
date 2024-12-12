{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Arrow (Arrow (second), first)
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (nub)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)

type Coord2D = (Int, Int)

type RegionForCoord = M.Map Coord2D (S.Set Coord2D)

type MatchingNeighbours = M.Map Coord2D (S.Set Coord2D)

data Garden where
  Garden ::
    { regionForCoord :: RegionForCoord,
      matchingNeighbours :: MatchingNeighbours
    } ->
    Garden
  deriving (Show, Eq)

parseRow :: A.Parser [(Coord2D, Char)]
parseRow = zipWith (\x -> ((x, 0),)) [0 ..] <$> A.many1 (A.notChar '\n')

parseMap :: A.Parser Garden
parseMap = toGarden . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    toGarden m =
      let fracturedRegions = M.mapWithKey (const . S.singleton) m
          coordsForPlant' = M.fromListWith S.union $ map (second S.singleton . swap) $ M.toList m
          matchingNeighbours' = M.mapWithKey (\c _ -> S.intersection (neighbours c) (coordsForPlant' M.! (m M.! c))) m
       in mergeRegions $ Garden fracturedRegions matchingNeighbours'

growRegionsStep :: Garden -> Garden
growRegionsStep g@(Garden regions matchingNeighbours) =
  let neighbourSet = S.map (matchingNeighbours M.!)
      expandedRegions = M.map (\r -> S.unions (S.insert r $ neighbourSet r)) regions
      normalisedRegions = M.fromListWith S.union $ map (\(_, r) -> (S.findMin r, r)) $ M.toList expandedRegions
   in g {regionForCoord = normalisedRegions}

mergeRegions :: Garden -> Garden
mergeRegions g =
  denormaliseRegions $
    fst $
      until (uncurry (==)) (join bimap growRegionsStep) (growRegionsStep g, g)
  where
    denormaliseRegions g@(Garden regions _) = g {regionForCoord = M.fromList $ concatMap (\(_, r) -> map (,r) $ S.toList r) $ M.toList regions}

perimeter :: Garden -> Coord2D -> Int
perimeter g c = 4 - length (matchingNeighbours g M.! c)

-- >>> part1 <$> loadInput "example.txt"
-- 1930
part1 :: Garden -> Int
part1 g = sum $ map (\r -> length r * S.foldl (\r c -> r + perimeter g c) 0 r) $ nub $ M.elems $ regionForCoord g

neighbours :: Coord2D -> S.Set Coord2D
neighbours (x, y) = S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

diagonals :: Coord2D -> S.Set Coord2D
diagonals (x, y) = S.fromList [(x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)]

cornerCountPoint :: Garden -> Coord2D -> Int
cornerCountPoint g c@(x, y) =
  let directNeighbours = matchingNeighbours g M.! c
      indirectNeighbours = S.intersection (regionForCoord g M.! c) $ diagonals (x, y)
      turnImplied = all ((== 2) . S.size . (`S.map` directNeighbours)) [fst, snd]
      corners = S.filter ((== 2) . S.size . S.intersection directNeighbours . neighbours) (diagonals c S.\\ indirectNeighbours)
   in case length directNeighbours of
        0 -> 4
        1 -> 2
        2
          | turnImplied ->
              let [(x', y'), (x'', y'')] = S.toList directNeighbours
                  innerCorner = (if x' == x then x'' else x', if y' == y then y'' else y')
               in if S.member innerCorner indirectNeighbours
                    then 1
                    else 2
        2 -> 0
        3 -> S.size corners
        4 -> S.size corners
        _ -> error "not possible"

cornerCountRegion :: Garden -> S.Set Coord2D -> Int
cornerCountRegion g = sum . map (cornerCountPoint g) . S.toList

-- >>> part2 <$> loadInput "example.txt"
-- 1206
part2 :: Garden -> Int
part2 g = sum $ map (\r -> length r * cornerCountRegion g r) $ nub $ M.elems $ regionForCoord g

loadInput :: [Char] -> IO Garden
loadInput = (fromRight (Garden M.empty M.empty) . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)
