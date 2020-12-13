{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , findPairWithSum
  , findTripletWithSum
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.List                      ( tails )
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet

-- Generate all unique pairs from a list input
pairs :: IntSet -> [(Int, Int)]
pairs intSet = [ (x, y) | (x : ys) <- tails $ IntSet.toList intSet, y <- ys ]

-- Optimised: Only search space that is within limit
potentialPairs :: Int -> IntSet -> [(Int, Int)]
potentialPairs targetSum intSet =
  [ (x, y)
  | (x : ys) <- tails $ IntSet.toAscList intSet
  , y        <- takeWhile (<= (targetSum - x)) ys
  ]

findPairWithSum :: Int -> IntSet -> Maybe (Int, Int)
findPairWithSum targetSum intSet = listToMaybe
  [ (x, y)
  | x <- IntSet.toList intSet
  , let y = targetSum - x
  , IntSet.member y intSet
  ]

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
  | let target = targetSum - IntSet.findMin intSet
  , (x, y) <- potentialPairs target intSet
  , let z = targetSum - (x + y)
  , IntSet.member z intSet
  ]

loadInput :: String -> IO IntSet
loadInput fileName =
  IntSet.fromList . map fst . mapMaybe BSC.readInt . BSC.lines <$> BSC.readFile
    ("src/" ++ fileName)
