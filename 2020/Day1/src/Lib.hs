module Lib
  ( pairs
  , loadInput
  , findPairWithSum
  , findTripletWithSum
  ) where

import           Data.List                      ( tails )
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( find )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet

-- Generate all unique pairs from a list input
pairs :: IntSet -> [(Int, Int)]
pairs intSet = [ (x, y) | (x : ys) <- tails $ IntSet.toList intSet, y <- ys ]


-- Naive: Generate all unique triplets from a list input
triplets :: IntSet -> [(Int, Int, Int)]
triplets intSet =
  [ (x, y, z)
  | (x : ys) <- tails $ IntSet.toList intSet
  , (y : zs) <- tails ys
  , z        <- zs
  ]

-- Optimised: use look-up of the partial-sum to find the last element
findTripletWithSum :: Int -> IntSet -> Maybe (Int, Int, Int)
findTripletWithSum targetSum intSet = listToMaybe
  [ (x, y, z)
  | (x, y) <- pairs intSet
  , let z = targetSum - (x + y)
  , IntSet.member z intSet
  ]

findPairWithSum :: (Foldable t, Eq a, Num a) => a -> t (a, a) -> Maybe (a, a)
findPairWithSum targetSum = find ((== targetSum) . (uncurry (+)))

loadInput :: String -> IO IntSet
loadInput fileName =
  IntSet.fromList . map read . lines <$> readFile ("src/" ++ fileName)
