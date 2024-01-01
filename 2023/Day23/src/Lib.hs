{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
    segmentsFromStart,
    longestViaCompressedGraph,
    compressGraph,
    printCompressedGraph,
    printGrid,
  )
where

import Control.Arrow (Arrow (second), first)
import Data
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (find, maximumBy, nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)

parseEntity :: A.Parser Entity
parseEntity = read . pure <$> A.satisfy (`elem` ("#.<>^v" :: String))

-- >>> A.parseOnly parseLine "###.....#.>.>.###.#.###"
-- Right [((0,0),#),((1,0),#),((2,0),#),((3,0),.),((4,0),.),((5,0),.),((6,0),.),((7,0),.),((8,0),#),((9,0),.),((10,0),>),((11,0),.),((12,0),>),((13,0),.),((14,0),#),((15,0),#),((16,0),#),((17,0),.),((18,0),#),((19,0),.),((20,0),#),((21,0),#),((22,0),#)]
parseLine :: A.Parser [Point]
parseLine = zipWith (\x e -> ((x, 0), e)) [0 ..] <$> A.many1' parseEntity

parseMap :: A.Parser Map
parseMap = M.unions . zipWith (\y -> M.filter (/= Forest) . M.fromList . map (first ((,y) . fst))) [0 ..] . reverse <$> (parseLine `A.sepBy1'` A.endOfLine)

-- >>> loadInput "example.txt"
-- fromList [((1,1),.),((1,2),.),((1,3),.),((1,5),.),((1,6),.),((1,7),.),((1,9),.),((1,10),.),((1,11),.),((1,12),.),((1,13),.),((1,21),.),((1,22),.),((2,1),.),((2,3),.),((2,5),.),((2,7),.),((2,9),.),((2,13),.),((2,21),.),((3,1),.),((3,3),.),((3,4),.),((3,5),.),((3,7),.),((3,9),.),((3,10),.),((3,11),.),((3,13),.),((3,15),.),((3,16),v),((3,17),.),((3,18),v),((3,19),.),((3,21),.),((4,1),.),((4,7),.),((4,11),.),((4,13),.),((4,15),.),((4,17),>),((4,19),.),((4,21),.),((5,1),.),((5,2),.),((5,3),.),((5,7),.),((5,8),v),((5,9),.),((5,10),v),((5,11),.),((5,13),.),((5,14),.),((5,15),.),((5,17),.),((5,19),.),((5,21),.),((6,3),.),((6,9),>),((6,17),.),((6,19),.),((6,21),.),((7,3),.),((7,4),.),((7,5),.),((7,7),.),((7,8),.),((7,9),.),((7,11),.),((7,12),.),((7,13),.),((7,14),.),((7,15),.),((7,16),.),((7,17),.),((7,19),.),((7,20),.),((7,21),.),((8,5),.),((8,7),.),((8,11),.),((9,1),.),((9,2),.),((9,3),.),((9,4),.),((9,5),.),((9,7),.),((9,8),.),((9,9),.),((9,11),.),((9,12),.),((9,13),.),((9,14),.),((9,15),.),((9,16),.),((9,17),.),((9,18),.),((9,19),.),((10,1),.),((10,9),.),((10,19),>),((11,1),.),((11,2),.),((11,3),.),((11,5),.),((11,6),.),((11,7),.),((11,9),.),((11,11),.),((11,12),.),((11,13),.),((11,15),.),((11,16),.),((11,17),.),((11,18),v),((11,19),.),((12,3),>),((12,5),.),((12,7),.),((12,9),>),((12,11),.),((12,13),.),((12,15),.),((12,19),>),((13,3),.),((13,4),v),((13,5),.),((13,7),.),((13,8),v),((13,9),.),((13,10),v),((13,11),.),((13,13),.),((13,15),.),((13,17),.),((13,18),.),((13,19),.),((14,3),>),((14,9),>),((14,13),.),((14,15),.),((14,17),.),((15,1),.),((15,2),.),((15,3),.),((15,5),.),((15,6),.),((15,7),.),((15,8),.),((15,9),.),((15,13),.),((15,15),.),((15,17),.),((16,1),.),((16,5),.),((16,13),.),((16,15),.),((16,17),.),((17,1),.),((17,2),.),((17,3),.),((17,5),.),((17,6),.),((17,7),.),((17,8),.),((17,9),.),((17,10),.),((17,11),.),((17,13),.),((17,14),.),((17,15),.),((17,17),.),((17,18),.),((17,19),.),((17,20),.),((17,21),.),((18,3),>),((18,11),.),((18,21),.),((19,1),.),((19,2),v),((19,3),.),((19,4),v),((19,5),.),((19,6),.),((19,7),.),((19,11),.),((19,13),.),((19,14),.),((19,15),.),((19,17),.),((19,18),.),((19,19),.),((19,20),.),((19,21),.),((20,1),.),((20,7),.),((20,11),>),((20,13),.),((20,15),.),((20,17),.),((21,0),.),((21,1),.),((21,7),.),((21,8),.),((21,9),.),((21,10),v),((21,11),.),((21,12),v),((21,13),.),((21,15),.),((21,16),.),((21,17),.)]
loadInput :: [Char] -> IO Map
loadInput = (fromRight M.empty . A.parseOnly parseMap <$>) . BSC.readFile . ("src/" ++)

-- >>>  generateSegments (1, 22) <$> loadInput "example.txt"
-- [MapSegment[entry=(1,22)](area=14)[exits=[((3,18),v)]],MapSegment[entry=(3,17)](area=1)[exits=[((3,16),v),((4,17),>)]],MapSegment[entry=(3,15)](area=19)[exits=[((5,10),v)]],MapSegment[entry=(5,9)](area=1)[exits=[((5,8),v),((6,9),>)]],MapSegment[entry=(5,7)](area=35)[exits=[((12,3),>)]],MapSegment[entry=(13,3)](area=1)[exits=[((14,3),>)]],MapSegment[entry=(15,3)](area=7)[exits=[((18,3),>)]],MapSegment[entry=(19,3)](area=1)[exits=[((19,2),v)]],MapSegment[entry=(19,1)](area=4)[exits=[]],MapSegment[entry=(7,9)](area=9)[exits=[((12,9),>)]],MapSegment[entry=(13,9)](area=1)[exits=[((13,8),v),((14,9),>)]],MapSegment[entry=(13,7)](area=7)[exits=[((13,4),v)]],MapSegment[entry=(13,3)](area=1)[exits=[((14,3),>)]],MapSegment[entry=(15,3)](area=7)[exits=[((18,3),>)]],MapSegment[entry=(19,3)](area=1)[exits=[((19,2),v)]],MapSegment[entry=(19,1)](area=4)[exits=[]],MapSegment[entry=(15,9)](area=15)[exits=[((20,11),>)]],MapSegment[entry=(21,11)](area=1)[exits=[((21,10),v)]],MapSegment[entry=(21,9)](area=7)[exits=[((19,4),v)]],MapSegment[entry=(19,3)](area=1)[exits=[((19,2),v)]],MapSegment[entry=(19,1)](area=4)[exits=[]],MapSegment[entry=(5,17)](area=19)[exits=[((10,19),>)]],MapSegment[entry=(11,19)](area=1)[exits=[((11,18),v),((12,19),>)]],MapSegment[entry=(11,17)](area=21)[exits=[((13,10),v)]],MapSegment[entry=(13,9)](area=1)[exits=[((13,8),v),((14,9),>)]],MapSegment[entry=(13,7)](area=7)[exits=[((13,4),v)]],MapSegment[entry=(13,3)](area=1)[exits=[((14,3),>)]],MapSegment[entry=(15,3)](area=7)[exits=[((18,3),>)]],MapSegment[entry=(19,3)](area=1)[exits=[((19,2),v)]],MapSegment[entry=(19,1)](area=4)[exits=[]],MapSegment[entry=(15,9)](area=15)[exits=[((20,11),>)]],MapSegment[entry=(21,11)](area=1)[exits=[((21,10),v)]],MapSegment[entry=(21,9)](area=7)[exits=[((19,4),v)]],MapSegment[entry=(19,3)](area=1)[exits=[((19,2),v)]],MapSegment[entry=(19,1)](area=4)[exits=[]],MapSegment[entry=(13,19)](area=27)[exits=[((21,12),v)]],MapSegment[entry=(21,11)](area=1)[exits=[((21,10),v)]],MapSegment[entry=(21,9)](area=7)[exits=[((19,4),v)]],MapSegment[entry=(19,3)](area=1)[exits=[((19,2),v)]],MapSegment[entry=(19,1)](area=4)[exits=[]]]
generateSegment :: Coord2D -> Map -> MapSegment
generateSegment c m = MapSegment c ps ss map
  where
    map = M.intersection m $ M.fromSet (const ()) ps <> M.map (const ()) ss
    ps = floodPath m c
    ss =
      M.fromList $
        filter (\(a, s) -> not (a `slide` s `S.member` ps)) $
          concatMap (mapMaybe ((\case (a, Just (Slope s)) -> Just (a, s); _ -> Nothing) . (\a -> (a, M.lookup a m))) . adjacents) $
            S.toList ps

slide :: Coord2D -> Slope -> Coord2D
slide c LeftSlope = first (subtract 1) c
slide c RightSlope = first (+ 1) c
slide c UpSlope = second (+ 1) c
slide c DownSlope = second (subtract 1) c

segmentsFromStart :: Map -> M.Map Coord2D MapSegment
segmentsFromStart m = mergeSegments $ M.fromList $ map (\s -> (entryPoint s, s)) $ generateSegments (minX, maxY) m
  where
    ((minX, _), (_, maxY)) = bounds m

mergeSegments :: M.Map Coord2D MapSegment -> M.Map Coord2D MapSegment
mergeSegments segments = fst $ head $ dropWhile (uncurry (/=)) $ zip steps (tail steps)
  where
    steps = iterate step segments
    step :: M.Map Coord2D MapSegment -> M.Map Coord2D MapSegment
    step s = case toMerge of
      Just seg ->
        let (slopeC, slope) = head $ M.toList $ slopes seg
            nextEntry = slide slopeC slope
            nextSegment = segments M.! nextEntry
            merged = MapSegment (entryPoint seg) (paths seg <> paths nextSegment <> M.keysSet (slopes seg)) (slopes nextSegment) (segment seg <> segment nextSegment)
         in M.insert (entryPoint seg) merged (M.delete nextEntry s)
      Nothing -> s
      where
        toMerge = find (\seg -> M.size (slopes seg) == 1) s

generateSegments :: Coord2D -> Map -> [MapSegment]
generateSegments c m =
  let segment@(MapSegment _ _ ss _) = generateSegment c m
   in nub $ segment : concatMap (\(sc, s) -> generateSegments (slide sc s) m) (M.toList ss)

adjacents :: Coord2D -> [Coord2D]
adjacents p = [first (+ 1) p, first (subtract 1) p, second (+ 1) p, second (subtract 1) p]

floodPath :: Map -> Coord2D -> S.Set Coord2D
floodPath m c = fst $ head $ dropWhile (not . S.null . snd) steps
  where
    steps = iterate floodStep (S.empty, S.singleton c)
    adjacentPaths = M.fromSet (filter ((== Just Path) . (`M.lookup` m)) . adjacents) $ M.keysSet m
    floodStep (v, frontier) =
      let frontier' = S.fromList (concatMap (fromMaybe [] . (`M.lookup` adjacentPaths)) $ S.toList frontier) S.\\ v
       in (v <> frontier, frontier')

-- <<loop>>
-- ]
longestHops :: MapSegment -> [PathState]
longestHops (MapSegment c ps ss _) = map (longestPathWithinSegment points c) $ S.toList goals
  where
    points = S.union ps goals
    goals =
      if M.null ss
        then S.singleton (maxX, minY)
        else S.union (M.keysSet ss) $ S.fromList $ map (uncurry slide) $ M.toList ss
    ((_, minY), (maxX, _)) = bounds $ M.fromSet (const ()) ps

-- >>> longestJourney True <$> loadInput "example.txt"
-- 95
longestJourney :: Map -> Int
longestJourney m = maximum $ solve state
  where
    hops = M.map longestHops $ segmentsFromStart m
    state =
      let s = (minX, maxY)
          e = (maxX, minY)
       in PathState s e s (S.singleton (minX, maxY))
    ((minX, minY), (maxX, maxY)) = bounds m
    options c h = filter ((`S.notMember` h) . end) $ fromMaybe [] $ M.lookup c hops
    solve (PathState s g c h)
      | c == g = [S.size h]
      | null (options c h) = []
      | otherwise = concatMap (solve . (\(PathState _ g' _ h') -> PathState s g g' (S.union h h'))) (options c h)

longestPathWithinSegment :: S.Set Coord2D -> Coord2D -> Coord2D -> PathState
longestPathWithinSegment points _s _g = maximumBy (compare `on` (S.size . history)) $ solve state
  where
    adj = M.fromSet (S.intersection points . S.fromList . adjacents) points
    state = PathState _s _g _s (S.singleton _s)
    options c h = adj M.! c S.\\ h
    solve s@(PathState _ g c h)
      | c == g = [s]
      | null (options c h) = []
      | otherwise = concatMap (solve . (\c' -> PathState _s g c' (S.insert c' h))) (options c h)

part1 :: Map -> Int
part1 = subtract 1 . longestJourney

part2 :: Map -> Int
part2 = longestViaCompressedGraph

longestViaCompressedGraph :: Map -> Int
longestViaCompressedGraph m = longestPathThroughCompressedGraph (M.keysSet m) (minX, maxY) (maxX, minY) - 1
  where
    ((minX, minY), (maxX, maxY)) = bounds m

-- >>> longestPathThroughCompressedGraph . M.keysSet <$> loadInput "example.txt"
longestPathThroughCompressedGraph :: S.Set Coord2D -> Coord2D -> Coord2D -> Int
longestPathThroughCompressedGraph points s g = maximum $ map (sum . map (\(_, _, c) -> c)) $ step fe [first] (S.fromList [fs, fe])
  where
    first@(fs, fe, fc) = head $ compressedLookup M.! s
    step :: Coord2D -> [CompressedEdge] -> S.Set Coord2D -> [[CompressedEdge]]
    step current path visited
      | current == g = [path]
      | otherwise =
          let options = filter (\(os, oe, _) -> S.notMember os visited && S.notMember oe visited) (adj M.! current)
           in case length options of
                0 -> []
                _ -> concatMap (\edge@(os, oe, _) -> step oe (edge : path) (S.union visited (S.fromList [os, oe, current]))) options
    adj = M.mapWithKey (\k _ -> nub $ concat $ mapMaybe (`M.lookup` compressedLookup) (filter (`S.member` points) $ adjacents k)) compressedLookup
    compressedLookup = M.map nub $ M.fromListWith (++) $ concatMap (\(s, e, c) -> [(s, pure (s, e, c)), (e, pure (e, s, c))]) $ compressGraph points

-- >>> compressGraph . M.keysSet <$> loadInput "example.txt"
-- [((3,17),(3,17),1),((3,16),(5,9),22),((3,18),(1,22),15),((4,17),(11,19),22),((5,9),(5,9),1),((5,8),(13,3),38),((5,10),(3,17),22),((6,9),(13,9),12),((11,19),(11,19),1),((10,19),(3,17),22),((11,18),(13,9),24),((12,19),(21,11),30),((13,3),(13,3),1),((12,3),(5,9),38),((13,4),(13,9),10),((14,3),(19,3),10),((13,9),(13,9),1),((12,9),(5,9),12),((13,8),(13,3),10),((13,10),(11,19),24),((14,9),(21,11),18),((19,3),(19,3),1),((18,3),(13,3),10),((19,2),(21,0),5),((19,4),(21,11),10),((21,11),(21,11),1),((20,11),(13,9),18),((21,10),(19,3),10),((21,12),(11,19),30)]
compressGraph :: S.Set Coord2D -> [CompressedEdge]
compressGraph points = concatMap (\c -> (c, c, 1) : map (\a -> followOnlyPath (a, a, 1) adj (S.singleton c)) (S.toList $ adj M.! c)) choices
  where
    adj = M.fromSet (S.intersection points . S.fromList . adjacents) points
    choices = S.filter ((> 2) . S.size . (adj M.!)) points

followOnlyPath :: CompressedEdge -> M.Map Coord2D (S.Set Coord2D) -> S.Set Coord2D -> CompressedEdge
followOnlyPath (s, e, c) adj visited =
  case S.toList (adj M.! e S.\\ visited) of
    [next] ->
      let visited' = S.insert e visited
       in if 1 < S.size (adj M.! next S.\\ visited') then (s, e, c) else followOnlyPath (s, next, c + 1) adj visited'
    _ -> (s, e, c)

printCompressedGraph :: Map -> String
printCompressedGraph m = printGrid $ M.union (M.fromList $ concatMap (\(s, e, c) -> [(s, show c), (e, show c)]) $ compressGraph $ M.keysSet m) (M.map show m)
