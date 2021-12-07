{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    cheapestPositionWithLinearCost,
    cheapestPositionWithGeometricCost,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.Function (on)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (minimumBy)

type CountsByPosition = M.IntMap Int

type PositionWithCost = (Int, Int)

-- >>> A.parseOnly parseCrabPositions "3,4,3,1,2"
-- Right [3,4,3,1,2]
parseCrabPositions :: A.Parser [Int]
parseCrabPositions = A.sepBy1 A.decimal ","

-- >>> crabRange $ M.fromList [(0,1),(1,2),(2,3),(4,1),(7,1),(14,1),(16,1)]
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
crabRange :: CountsByPosition -> [Int]
crabRange countsByPosition = [S.findMin positions .. S.findMax positions]
  where
    positions = M.keysSet countsByPosition

costsToAlign :: (Int -> Int -> Int) -> CountsByPosition -> [(Int, Int)]
costsToAlign scoringFunction countsByPosition = zip options $ map costForPosition options
  where
    options = crabRange countsByPosition
    costForPosition targetPosition = sum $ M.elems $ M.mapWithKey (\position count -> scoringFunction (abs (targetPosition - position)) count) countsByPosition

-- >>> costsToAlignLinear $ M.fromList [(0,1),(1,2),(2,3),(4,1),(7,1),(14,1),(16,1)]
-- [(0,49),(1,41),(2,37),(3,39),(4,41),(5,45),(6,49),(7,53),(8,59),(9,65),(10,71),(11,77),(12,83),(13,89),(14,95),(15,103),(16,111)]
costsToAlignLinear :: CountsByPosition -> [PositionWithCost]
costsToAlignLinear = costsToAlign (*)

-- >>> costsToAlignGeometric $ M.fromList [(0,1),(1,2),(2,3),(4,1),(7,1),(14,1),(16,1)]
-- [(0,290),(1,242),(2,206),(3,183),(4,170),(5,168),(6,176),(7,194),(8,223),(9,262),(10,311),(11,370),(12,439),(13,518),(14,607),(15,707),(16,817)]
costsToAlignGeometric :: CountsByPosition -> [PositionWithCost]
costsToAlignGeometric = costsToAlign (\distance count -> sum [1 .. distance] * count)

-- >>> cheapestPositionWithLinearCost $ M.fromList [(0,1),(1,2),(2,3),(4,1),(7,1),(14,1),(16,1)]
-- (2,37)
cheapestPositionWithLinearCost :: CountsByPosition -> PositionWithCost
cheapestPositionWithLinearCost = minimumBy (compare `on` snd) . costsToAlignLinear

-- >>> cheapestPositionWithGeometricCost $ M.fromList [(0,1),(1,2),(2,3),(4,1),(7,1),(14,1),(16,1)]
-- (5,168)
cheapestPositionWithGeometricCost :: CountsByPosition -> PositionWithCost
cheapestPositionWithGeometricCost = minimumBy (compare `on` snd) . costsToAlignGeometric

-- >>> loadInput "example.txt"
-- fromList [(0,1),(1,2),(2,3),(4,1),(7,1),(14,1),(16,1)]
loadInput :: Num a => [Char] -> IO (M.IntMap a)
loadInput fileName =
  M.unionsWith (+) . map (uncurry M.singleton) . flip zip (repeat 1) . fromRight []
    . A.parseOnly parseCrabPositions
    <$> BSC.readFile
      ("src/" ++ fileName)
