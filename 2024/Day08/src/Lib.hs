{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Arrow (first)
import Data (Antinode (Initial, Resonant), Bounds, Coord2D, Frequency, Space (Space, antiNodes, tiles), Tile (Antenna, frequency), bounds)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (tails)
import Data.Map qualified as M
import Data.Set qualified as S

-- >>> A.parseOnly parseRow "........0..."
-- Right [((0,0),'.'),((1,0),'.'),((2,0),'.'),((3,0),'.'),((4,0),'.'),((5,0),'.'),((6,0),'.'),((7,0),'.'),((8,0),'0'),((9,0),'.'),((10,0),'.'),((11,0),'.')]
parseRow :: A.Parser [(Coord2D, Tile)]
parseRow = zipWith (\x -> ((x, 0),)) [0 ..] . map (read . pure) <$> A.many1 (A.notChar '\n')

parseMap :: A.Parser Space
parseMap = toSpace . M.unions . zipWith (\y -> M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseRow `A.sepBy1` A.endOfLine)
  where
    toSpace as = Space as (computeAntinodes as)

-- >>> groupAntennasByFrequency . tiles <$> loadInput "example.txt"
-- fromList [('0',[(8,10),(7,8),(5,9),(4,7)]),('A',[(9,2),(8,3),(6,6)])]
groupAntennasByFrequency :: M.Map k Tile -> M.Map Frequency [k]
groupAntennasByFrequency = M.foldlWithKey accumulate M.empty . M.filter (\case Antenna _ -> True; _ -> False)
  where
    accumulate acc k v = M.insertWith (++) (frequency v) [k] acc

computeAntinodes :: M.Map Coord2D Tile -> S.Set Antinode
computeAntinodes ts = onlyStrongest $ S.unions $ map (antinodesForFrequency (bounds ts)) $ M.elems $ groupAntennasByFrequency ts
  where
    onlyStrongest antinodes =
      let (initial, resonant) = S.partition (\case Initial _ -> True; _ -> False) antinodes
       in S.union initial (resonant S.\\ S.map (\case Initial c -> Resonant c; n -> n) initial)

antinodesForFrequency :: Bounds -> [Coord2D] -> S.Set Antinode
antinodesForFrequency ((minX, minY), (maxX, maxY)) = S.unions . map antinodesForPair . pairs
  where
    pairwise f (x, y) (x', y') = (f x x', f y y')
    inBounds (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY
    resonate d (dx, dy) = takeWhile inBounds . iterate (pairwise (+) (d * dx, d * dy))
    mapHeadTail _ _ [] = []
    mapHeadTail f g (x : xs) = f x : map g xs
    antinodesForPair ((x, y), (x', y')) =
      let (dx, dy) = pairwise (-) (x', y') (x, y)
          resonateUp = resonate 1 (dx, dy) (x' + dx, y' + dy)
          resonateDown = resonate (-1) (dx, dy) (x - dx, y - dy)
       in S.fromList $ Resonant (x, y) : Resonant (x', y') : concatMap (mapHeadTail Initial Resonant) [resonateUp, resonateDown]
    pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

-- >>> loadInput "example.txt"
-- ▪▪....#....#
-- .▪.#....0...
-- ..▪.#0....#.
-- ..#▪...0....
-- ....0....#..
-- .#...▪A....▪
-- ...#..▪.....
-- #....▪.#....
-- ..▪.....A...
-- ....▪....A..
-- .▪........#.
-- ...▪......#▪

-- >>> part1 <$> loadInput "example.txt"
-- 14
part1 :: Space -> Int
part1 = S.size . S.filter (\case Initial _ -> True; _ -> False) . antiNodes

-- >>> loadInput "example2.txt"
-- T....#....
-- ...T......
-- .T....#...
-- .........▪
-- ..#.......
-- ..........
-- ...▪......
-- ..........
-- ....▪.....
-- ..........

-- >>> part2 <$> loadInput "example.txt"
-- 34
part2 :: Space -> Int
part2 = S.size . antiNodes

loadInput :: [Char] -> IO Space
loadInput = (fromRight (Space M.empty S.empty) . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)
