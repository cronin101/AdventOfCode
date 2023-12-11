{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
    printInput,
    markSquares,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (find, intercalate)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as S

type Coord2D = (Int, Int)

type PipeGrid = M.Map Coord2D Pipe

data CardinalDirection = North | South | East | West deriving (Show, Eq, Ord)

data SearchState = SearchState {frontier :: S.Set Pipe, distanceForCoord :: M.Map Coord2D Int} deriving (Show, Eq, Ord)

data Color = Black | White deriving (Show, Eq, Ord)

data Pipe = Pipe
  { pipeChar :: Char,
    pipeLocation :: Coord2D,
    pipeConnections :: [CardinalDirection]
  }
  deriving (Show, Eq, Ord)

pipeCharToDirections :: Char -> Maybe [CardinalDirection]
pipeCharToDirections c = case c of
  'S' -> Just [North, South, East, West]
  '|' -> Just [North, South]
  '-' -> Just [East, West]
  'L' -> Just [North, East]
  'J' -> Just [North, West]
  '7' -> Just [South, West]
  'F' -> Just [South, East]
  _ -> Nothing

neighbours :: PipeGrid -> Pipe -> S.Set Pipe
neighbours pipegrid (Pipe _ (x, y) cs) = S.fromList $ mapMaybe neighbour cs
  where
    neighbour l =
      M.lookup
        ( case l of
            North -> (x, y + 1)
            South -> (x, y - 1)
            East -> (x + 1, y)
            West -> (x - 1, y)
        )
        pipegrid

-- >>> A.parseOnly parsePipe "S"
-- Right (Just (Pipe {pipeChar = 'S', pipeLocation = (0,0), pipeConnections = [North,South,East,West]}))

-- >>> A.parseOnly parsePipe "|"
-- Right (Just (Pipe {pipeChar = '|', pipeLocation = (0,0), pipeConnections = [North,South]}))

-- >>> A.parseOnly parsePipe "."
-- Right Nothing
parsePipe :: A.Parser (Maybe Pipe)
parsePipe = (\c -> Pipe c (0, 0) <$> pipeCharToDirections c) <$> A.notChar '\n'

-- >>> A.parseOnly parsePipeRow "L-77.L-J7F.-7FFF77.F7-|7.7-F-7L--|-|-7-F|FL7FF|.7FF-L7LFJ77-J--|.F7.-FF-"
-- Right [Pipe {pipeChar = 'L', pipeLocation = (0,0), pipeConnections = [North,East]},Pipe {pipeChar = '-', pipeLocation = (1,0), pipeConnections = [East,West]},Pipe {pipeChar = '7', pipeLocation = (2,0), pipeConnections = [South,West]},Pipe {pipeChar = '7', pipeLocation = (3,0), pipeConnections = [South,West]},Pipe {pipeChar = 'L', pipeLocation = (5,0), pipeConnections = [North,East]},Pipe {pipeChar = '-', pipeLocation = (6,0), pipeConnections = [East,West]},Pipe {pipeChar = 'J', pipeLocation = (7,0), pipeConnections = [North,West]},Pipe {pipeChar = '7', pipeLocation = (8,0), pipeConnections = [South,West]},Pipe {pipeChar = 'F', pipeLocation = (9,0), pipeConnections = [South,East]},Pipe {pipeChar = '-', pipeLocation = (11,0), pipeConnections = [East,West]},Pipe {pipeChar = '7', pipeLocation = (12,0), pipeConnections = [South,West]},Pipe {pipeChar = 'F', pipeLocation = (13,0), pipeConnections = [South,East]},Pipe {pipeChar = 'F', pipeLocation = (14,0), pipeConnections = [South,East]},Pipe {pipeChar = 'F', pipeLocation = (15,0), pipeConnections = [South,East]},Pipe {pipeChar = '7', pipeLocation = (16,0), pipeConnections = [South,West]},Pipe {pipeChar = '7', pipeLocation = (17,0), pipeConnections = [South,West]},Pipe {pipeChar = 'F', pipeLocation = (19,0), pipeConnections = [South,East]},Pipe {pipeChar = '7', pipeLocation = (20,0), pipeConnections = [South,West]},Pipe {pipeChar = '-', pipeLocation = (21,0), pipeConnections = [East,West]},Pipe {pipeChar = '|', pipeLocation = (22,0), pipeConnections = [North,South]},Pipe {pipeChar = '7', pipeLocation = (23,0), pipeConnections = [South,West]},Pipe {pipeChar = '7', pipeLocation = (25,0), pipeConnections = [South,West]},Pipe {pipeChar = '-', pipeLocation = (26,0), pipeConnections = [East,West]},Pipe {pipeChar = 'F', pipeLocation = (27,0), pipeConnections = [South,East]},Pipe {pipeChar = '-', pipeLocation = (28,0), pipeConnections = [East,West]},Pipe {pipeChar = '7', pipeLocation = (29,0), pipeConnections = [South,West]},Pipe {pipeChar = 'L', pipeLocation = (30,0), pipeConnections = [North,East]},Pipe {pipeChar = '-', pipeLocation = (31,0), pipeConnections = [East,West]},Pipe {pipeChar = '-', pipeLocation = (32,0), pipeConnections = [East,West]},Pipe {pipeChar = '|', pipeLocation = (33,0), pipeConnections = [North,South]},Pipe {pipeChar = '-', pipeLocation = (34,0), pipeConnections = [East,West]},Pipe {pipeChar = '|', pipeLocation = (35,0), pipeConnections = [North,South]},Pipe {pipeChar = '-', pipeLocation = (36,0), pipeConnections = [East,West]},Pipe {pipeChar = '7', pipeLocation = (37,0), pipeConnections = [South,West]},Pipe {pipeChar = '-', pipeLocation = (38,0), pipeConnections = [East,West]},Pipe {pipeChar = 'F', pipeLocation = (39,0), pipeConnections = [South,East]},Pipe {pipeChar = '|', pipeLocation = (40,0), pipeConnections = [North,South]},Pipe {pipeChar = 'F', pipeLocation = (41,0), pipeConnections = [South,East]},Pipe {pipeChar = 'L', pipeLocation = (42,0), pipeConnections = [North,East]},Pipe {pipeChar = '7', pipeLocation = (43,0), pipeConnections = [South,West]},Pipe {pipeChar = 'F', pipeLocation = (44,0), pipeConnections = [South,East]},Pipe {pipeChar = 'F', pipeLocation = (45,0), pipeConnections = [South,East]},Pipe {pipeChar = '|', pipeLocation = (46,0), pipeConnections = [North,South]},Pipe {pipeChar = '7', pipeLocation = (48,0), pipeConnections = [South,West]},Pipe {pipeChar = 'F', pipeLocation = (49,0), pipeConnections = [South,East]},Pipe {pipeChar = 'F', pipeLocation = (50,0), pipeConnections = [South,East]},Pipe {pipeChar = '-', pipeLocation = (51,0), pipeConnections = [East,West]},Pipe {pipeChar = 'L', pipeLocation = (52,0), pipeConnections = [North,East]},Pipe {pipeChar = '7', pipeLocation = (53,0), pipeConnections = [South,West]},Pipe {pipeChar = 'L', pipeLocation = (54,0), pipeConnections = [North,East]},Pipe {pipeChar = 'F', pipeLocation = (55,0), pipeConnections = [South,East]},Pipe {pipeChar = 'J', pipeLocation = (56,0), pipeConnections = [North,West]},Pipe {pipeChar = '7', pipeLocation = (57,0), pipeConnections = [South,West]},Pipe {pipeChar = '7', pipeLocation = (58,0), pipeConnections = [South,West]},Pipe {pipeChar = '-', pipeLocation = (59,0), pipeConnections = [East,West]},Pipe {pipeChar = 'J', pipeLocation = (60,0), pipeConnections = [North,West]},Pipe {pipeChar = '-', pipeLocation = (61,0), pipeConnections = [East,West]},Pipe {pipeChar = '-', pipeLocation = (62,0), pipeConnections = [East,West]},Pipe {pipeChar = '|', pipeLocation = (63,0), pipeConnections = [North,South]},Pipe {pipeChar = 'F', pipeLocation = (65,0), pipeConnections = [South,East]},Pipe {pipeChar = '7', pipeLocation = (66,0), pipeConnections = [South,West]},Pipe {pipeChar = '-', pipeLocation = (68,0), pipeConnections = [East,West]},Pipe {pipeChar = 'F', pipeLocation = (69,0), pipeConnections = [South,East]},Pipe {pipeChar = 'F', pipeLocation = (70,0), pipeConnections = [South,East]},Pipe {pipeChar = '-', pipeLocation = (71,0), pipeConnections = [East,West]}]
parsePipeRow :: A.Parser [Pipe]
parsePipeRow =
  mapMaybe maybeSetXLocation
    . zip [0 ..]
    <$> A.many1 parsePipe
  where
    maybeSetXLocation (x, pipe) = fmap (\p -> p {pipeLocation = (x, 0)}) pipe

parsePipeGrid :: A.Parser PipeGrid
parsePipeGrid =
  M.fromList
    . concat
    . zipWith toTupleWithCoordinates [0 ..]
    . reverse
    . filter (not . null)
    <$> parsePipeRow `A.sepBy1` A.endOfLine
  where
    toTupleWithCoordinates y = map (\pipe -> let x = fst $ pipeLocation pipe in ((x, y), pipe {pipeLocation = (x, y)}))

-- >>> loadInput "example1.txt"
-- fromList [((0,0),Pipe {pipeChar = 'L', pipeLocation = (0,0), pipeConnections = [North,East]}),((0,1),Pipe {pipeChar = '-', pipeLocation = (0,1), pipeConnections = [East,West]}),((0,2),Pipe {pipeChar = 'L', pipeLocation = (0,2), pipeConnections = [North,East]}),((0,3),Pipe {pipeChar = '7', pipeLocation = (0,3), pipeConnections = [South,West]}),((0,4),Pipe {pipeChar = '-', pipeLocation = (0,4), pipeConnections = [East,West]}),((1,0),Pipe {pipeChar = '|', pipeLocation = (1,0), pipeConnections = [North,South]}),((1,1),Pipe {pipeChar = 'L', pipeLocation = (1,1), pipeConnections = [North,East]}),((1,2),Pipe {pipeChar = '|', pipeLocation = (1,2), pipeConnections = [North,South]}),((1,3),Pipe {pipeChar = 'S', pipeLocation = (1,3), pipeConnections = [North,South,East,West]}),((1,4),Pipe {pipeChar = 'L', pipeLocation = (1,4), pipeConnections = [North,East]}),((2,0),Pipe {pipeChar = '-', pipeLocation = (2,0), pipeConnections = [East,West]}),((2,1),Pipe {pipeChar = '-', pipeLocation = (2,1), pipeConnections = [East,West]}),((2,2),Pipe {pipeChar = '7', pipeLocation = (2,2), pipeConnections = [South,West]}),((2,3),Pipe {pipeChar = '-', pipeLocation = (2,3), pipeConnections = [East,West]}),((2,4),Pipe {pipeChar = '|', pipeLocation = (2,4), pipeConnections = [North,South]}),((3,0),Pipe {pipeChar = 'J', pipeLocation = (3,0), pipeConnections = [North,West]}),((3,1),Pipe {pipeChar = 'J', pipeLocation = (3,1), pipeConnections = [North,West]}),((3,2),Pipe {pipeChar = '|', pipeLocation = (3,2), pipeConnections = [North,South]}),((3,3),Pipe {pipeChar = '7', pipeLocation = (3,3), pipeConnections = [South,West]}),((3,4),Pipe {pipeChar = 'F', pipeLocation = (3,4), pipeConnections = [South,East]}),((4,0),Pipe {pipeChar = 'F', pipeLocation = (4,0), pipeConnections = [South,East]}),((4,1),Pipe {pipeChar = '|', pipeLocation = (4,1), pipeConnections = [North,South]}),((4,2),Pipe {pipeChar = '|', pipeLocation = (4,2), pipeConnections = [North,South]}),((4,3),Pipe {pipeChar = '|', pipeLocation = (4,3), pipeConnections = [North,South]}),((4,4),Pipe {pipeChar = '7', pipeLocation = (4,4), pipeConnections = [South,West]})]
loadInput :: [Char] -> IO PipeGrid
loadInput = (fromRight M.empty . A.parseOnly parsePipeGrid <$>) . BSC.readFile . ("src/" ++)

solve :: PipeGrid -> SearchState -> M.Map Coord2D Int
solve grid state@(SearchState f d)
  | S.null f = d
  | otherwise = solve grid $ step grid state

step :: PipeGrid -> SearchState -> SearchState
step grid (SearchState f dfc) = SearchState frontier' distanceForCoord'
  where
    validNeighbours p = S.filter (S.member p . neighbours grid) $ neighbours grid p
    neighboursWithIncrementedDistance p@(Pipe _ l _) = M.fromList $ map ((,1 + dfc M.! l) . pipeLocation) $ S.toList $ validNeighbours p
    nextPipesWithDistances = M.unionsWith min $ map neighboursWithIncrementedDistance $ S.toList f
    frontier' = S.map (grid M.!) $ M.keysSet nextPipesWithDistances S.\\ M.keysSet dfc
    distanceForCoord' = M.unionWith min dfc nextPipesWithDistances

-- >>> part1 <$> loadInput "example1.txt"
-- 4

-- >>> part1 <$> loadInput "example2.txt"
-- 8
solveGrid :: PipeGrid -> M.Map Coord2D Int
solveGrid grid = solve grid (SearchState (S.singleton start) (M.singleton (pipeLocation start) 0))
  where
    Just (_, start) = find ((== 'S') . pipeChar . snd) $ M.toList grid

part1 :: PipeGrid -> Int
part1 = maximum . M.elems . solveGrid

-- >>> part2 <$> loadInput "example2.txt"
-- 1

-- >>> part2 <$> loadInput "example3.txt"
-- 4

-- >>> part2 <$> loadInput "example4.txt"
-- 8
part2 :: PipeGrid -> Int
part2 = M.size . M.filter ((== '●') . pipeChar) . markSquares

maxX :: Ord a1 => M.Map (a1, b) a2 -> a1
maxX m = maximum (map fst (M.keys m))

minX :: Ord a1 => M.Map (a1, b) a2 -> a1
minX m = minimum (map fst (M.keys m))

maxY :: Ord a1 => M.Map (a2, a1) a3 -> a1
maxY m = maximum (map snd (M.keys m))

minY :: Ord a1 => M.Map (a2, a1) a3 -> a1
minY m = minimum (map snd (M.keys m))

printGrid :: M.Map Coord2D String -> String
printGrid m = intercalate "\n" ([concat [(\s -> (if length s == 1 then "  " else " ") ++ s) $ fromMaybe " " (M.lookup (x, y) m) | x <- [0 .. maxX m]] | y <- [maxY m, maxY m - 1 .. 0]])

printInput :: PipeGrid -> String
printInput = printGrid . M.map ((" " ++) . pure . pipeChar)

-- >>> onlyVerticals "FJ||||||||L7"
-- "||||||||||"
onlyVerticals :: String -> String
onlyVerticals "" = ""
onlyVerticals ('-' : s) = onlyVerticals s
onlyVerticals (x : '-' : s) = onlyVerticals (x : s)
onlyVerticals ('|' : s) = '|' : onlyVerticals s
onlyVerticals ('F' : 'J' : s) = '|' : onlyVerticals s
onlyVerticals ('L' : '7' : s) = '|' : onlyVerticals s
onlyVerticals (_ : s) = onlyVerticals s

onlyMainLoop :: PipeGrid -> PipeGrid
onlyMainLoop g = fixS $ M.filter ((`M.member` solveGrid g) . pipeLocation) g
  where
    fixS = M.insert sLocation fixedS
    sLocation@(x, y) = fst $ head $ filter ((== 'S') . pipeChar . snd) $ M.toList g
    aboveConnectsDown = maybe False (S.member South . S.fromList . pipeConnections) (M.lookup (x, y + 1) g)
    belowConnectsUp = maybe False (S.member North . S.fromList . pipeConnections) (M.lookup (x, y - 1) g)
    leftConnectsRight = maybe False (S.member East . S.fromList . pipeConnections) (M.lookup (x - 1, y) g)
    rightConnectsLeft = maybe False (S.member West . S.fromList . pipeConnections) (M.lookup (x + 1, y) g)
    fixedS
      | aboveConnectsDown && belowConnectsUp = Pipe '|' sLocation [North, South]
      | leftConnectsRight && rightConnectsLeft = Pipe '-' sLocation [West, East]
      | aboveConnectsDown && rightConnectsLeft = Pipe 'L' sLocation [North, East]
      | aboveConnectsDown && leftConnectsRight = Pipe 'J' sLocation [North, West]
      | belowConnectsUp && leftConnectsRight = Pipe '7' sLocation [South, West]
      | belowConnectsUp && rightConnectsLeft = Pipe 'F' sLocation [South, East]
      | otherwise = error "unhandled S"

-- Returns if a point is inside the PipeGrid
rayTrace :: PipeGrid -> Coord2D -> Bool
rayTrace g (x, y) = odd verticalCount
  where
    verticalCount = length $ onlyVerticals charsRight
    charsRight = catMaybes [pipeChar <$> (x', y) `M.lookup` g | x' <- [x + 1 .. maxX g]]

markSquares :: PipeGrid -> PipeGrid
markSquares g = solve' unmarkedPixels state
  where
    solve' remaining state'
      | S.null remaining = state'
      | otherwise =
          let pixel = S.findMin remaining
              pixelChar = if rayTrace mainLoop pixel then '●' else '○'
              floodedFromPixel = floodFill pixelChar (S.singleton pixel) state'
           in solve' (remaining S.\\ M.keysSet floodedFromPixel) floodedFromPixel
    unmarkedPixels = S.fromList [(x, y) | x <- [minX g .. maxX g], y <- [minY g .. maxY g], not $ M.member (x, y) state]
    mainLoop = onlyMainLoop g
    state = floodFill '○' (perimeter g) mainLoop

perimeter :: PipeGrid -> S.Set Coord2D
perimeter g = S.fromList (concat ([[(minX g, y), (maxX g, y)] | y <- [minY g .. maxY g]] ++ [[(x, minY g), (x, maxY g)] | x <- [minX g .. maxX g]])) S.\\ M.keysSet g

floodFill :: Char -> S.Set Coord2D -> PipeGrid -> PipeGrid
floodFill char initFrontier baseGrid = floodFill' initGrid initFrontier S.empty
  where
    initGrid = M.union (M.fromList $ map (\l -> (l, Pipe char l [])) $ S.toList initFrontier) baseGrid
    floodFill' grid frontier visited
      | S.null frontier = grid
      | otherwise = floodFill' nextGrid frontier' (S.union visited frontier)
      where
        frontier' = S.unions (S.map neighbourSet frontier) S.\\ visited
        inbounds (x, y) = x >= minX baseGrid && x <= maxX baseGrid && y >= minY baseGrid && y <= maxY baseGrid
        neighbourSet (x, y) = S.filter (\c -> not (c `M.member` grid) && inbounds c) $ adjacents (x, y)
        adjacents (x, y) = S.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        nextGrid = M.union (M.fromList $ map (\l -> (l, Pipe char l [])) $ S.toList (frontier' S.\\ M.keysSet baseGrid)) grid
