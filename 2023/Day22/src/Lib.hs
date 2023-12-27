{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (partition, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as S

type Coord3D = (Int, Int, Int)

type XYCoord2D = (Int, Int)

type HeightMap = M.Map XYCoord2D Int

type PointToBlockMap = M.Map Coord3D RestingBlock

data RestingBlock = RestingBlock
  { blockPoints :: S.Set Coord3D,
    blockRestingOn :: S.Set RestingBlock
  }
  deriving (Show, Eq, Ord)

type BlockAsPoints = S.Set Coord3D

type Input = [BlockAsPoints]

data AssemblingState = AssemblingState
  { stillToFall :: [BlockAsPoints],
    restingBlocks :: [RestingBlock],
    heightMap :: HeightMap,
    pointToBlockMap :: PointToBlockMap
  }
  deriving (Show)

data FallingState = FallingState
  { falling :: S.Set RestingBlock,
    resting :: S.Set RestingBlock
  }
  deriving (Show, Eq)

-- >>> A.parseOnly parseCoord3D "1,0,1"
-- Right (1,0,1)
parseCoord3D :: A.Parser Coord3D
parseCoord3D = do
  [x, y, z] <- A.decimal `A.sepBy1` ","
  return (x, y, z)

-- >>> A.parseOnly parseBlockAsPoints "1,0,1~2,1,1"
-- Right (fromList [(1,0,1),(1,1,1),(2,0,1),(2,1,1)])
parseBlockAsPoints :: A.Parser BlockAsPoints
parseBlockAsPoints = do
  ((xLo, yLo, zLo), (xHi, yHi, zHi)) <- (,) <$> (parseCoord3D <* "~") <*> parseCoord3D
  return $ S.fromList [(x, y, z) | x <- [xLo .. xHi], y <- [yLo .. yHi], z <- [zLo .. zHi]]

parseBlocksAsPoints :: A.Parser Input
parseBlocksAsPoints = parseBlockAsPoints `A.sepBy1` A.endOfLine

-- >>> loadInput "example.txt"
-- [fromList [(1,0,1),(1,1,1),(1,2,1)],fromList [(0,0,2),(1,0,2),(2,0,2)],fromList [(0,2,3),(1,2,3),(2,2,3)],fromList [(0,0,4),(0,1,4),(0,2,4)],fromList [(2,0,5),(2,1,5),(2,2,5)],fromList [(0,1,6),(1,1,6),(2,1,6)],fromList [(1,1,8),(1,1,9)]]
loadInput :: [Char] -> IO Input
loadInput = (fromRight [] . A.parseOnly parseBlocksAsPoints <$>) . BSC.readFile . ("src/" ++)

-- >>> initialize <$> loadInput "example.txt"
-- State {stillToFall = [fromList [(1,0,1),(1,1,1),(1,2,1)],fromList [(0,0,2),(1,0,2),(2,0,2)],fromList [(0,2,3),(1,2,3),(2,2,3)],fromList [(0,0,4),(0,1,4),(0,2,4)],fromList [(2,0,5),(2,1,5),(2,2,5)],fromList [(0,1,6),(1,1,6),(2,1,6)],fromList [(1,1,8),(1,1,9)]], restingBlocks = [], heightMap = fromList [], pointToBlockMap = fromList []}
initialize :: Input -> AssemblingState
initialize stf = AssemblingState {stillToFall = stf', restingBlocks = [], heightMap = M.empty, pointToBlockMap = M.empty}
  where
    -- Gotcha! Bricks aren't falling in the order they're given in the input file.
    stf' = sortBy (compare `on` (S.findMin . S.map (\(_, _, z) -> z))) stf

-- >>> assemble . initialize <$> loadInput "example.txt"
-- State {stillToFall = [fromList [(0,0,2),(1,0,2),(2,0,2)],fromList [(0,2,3),(1,2,3),(2,2,3)],fromList [(0,0,4),(0,1,4),(0,2,4)],fromList [(2,0,5),(2,1,5),(2,2,5)],fromList [(0,1,6),(1,1,6),(2,1,6)],fromList [(1,1,8),(1,1,9)]], restingBlocks = [RestingBlock {blockPoints = fromList [(1,0,1),(1,1,1),(1,2,1)], blockRestingOn = fromList []}], heightMap = fromList [((1,0),1),((1,1),1),((1,2),1)], pointToBlockMap = fromList [((1,0,1),RestingBlock {blockPoints = fromList [(1,0,1),(1,1,1),(1,2,1)], blockRestingOn = fromList []}),((1,1,1),RestingBlock {blockPoints = fromList [(1,0,1),(1,1,1),(1,2,1)], blockRestingOn = fromList []}),((1,2,1),RestingBlock {blockPoints = fromList [(1,0,1),(1,1,1),(1,2,1)], blockRestingOn = fromList []})]}
assemble :: AssemblingState -> AssemblingState
assemble s@(AssemblingState {stillToFall = []}) = s
assemble s@(AssemblingState (b : bs) rBs hM pTBM) =
  AssemblingState
    bs
    (restingBlock : rBs)
    (M.unionWith max hM heightMapOfBlock)
    (M.union pTBM $ M.fromSet (const restingBlock) (blockPoints restingBlock))
  where
    restingBlock = restBlock b s
    heightMapOfBlock = M.fromListWith max $ map (\(x, y, z) -> ((x, y), z)) $ S.toList $ blockPoints restingBlock

restBlock :: BlockAsPoints -> AssemblingState -> RestingBlock
restBlock points (AssemblingState _ _ hM pTBM) = RestingBlock newPoints restingOn
  where
    heightMapIntersections =
      let xyCoords = M.fromSet (const ()) $ S.map (\(x, y, _) -> (x, y)) points
       in M.intersection hM xyCoords
    restingHeight = 1 + maximum (0 : M.elems heightMapIntersections)
    heightMapCeiling = M.filter (== restingHeight - 1) heightMapIntersections
    newPoints =
      let minZ = minimum $ S.map (\(_, _, z) -> z) points
          fallDelta = minZ - restingHeight
       in S.map (\(x, y, z) -> (x, y, z - fallDelta)) points
    restingOn = S.map fromJust $ S.filter isJust $ S.map (\(x, y) -> M.lookup (x, y, restingHeight - 1) pTBM) $ M.keysSet heightMapCeiling

assembleAll :: AssemblingState -> AssemblingState
assembleAll s = head $ dropWhile (not . null . stillToFall) steps
  where
    steps = iterate assemble s

necessaryUnnecessaryBricks :: AssemblingState -> ([RestingBlock], [RestingBlock])
necessaryUnnecessaryBricks (AssemblingState _ rBs _ _) = partition (\b -> any ((== S.singleton b) . blockRestingOn) rBs) rBs

part1 :: Input -> Int
part1 = length . snd . necessaryUnnecessaryBricks . assembleAll . initialize

stepFalling :: FallingState -> FallingState
stepFalling (FallingState f r) = FallingState falling' resting'
  where
    falling' = S.union f $ S.filter (\b -> S.null (blockRestingOn b S.\\ f)) r
    resting' = r S.\\ falling'

afterAllFalling :: FallingState -> FallingState
afterAllFalling s = fst $ head $ dropWhile (uncurry (/=) . bimap (length . falling) (length . falling)) $ zip steps (tail steps)
  where
    steps = iterate stepFalling s

-- >>> part2 <$> loadInput "example.txt"
-- 41
part2 :: Input -> Int
part2 i = sum $ map dependentCount necessary
  where
    (necessary, unnecessary) = necessaryUnnecessaryBricks $ assembleAll $ initialize i
    raisedBlocks = S.filter (\b -> S.findMin (S.map (\(_, _, z) -> z) $ blockPoints b) > 1) $ S.fromList (necessary ++ unnecessary)
    dependentCount b = S.size (falling (afterAllFalling $ FallingState (S.singleton b) raisedBlocks)) - 1
