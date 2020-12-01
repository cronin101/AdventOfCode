module Lib
  ( pairs
  , triplets'
  , loadInput
  , findPairWithSum
  , findTripletWithSum
  ) where

import           Data.List                      ( tails
                                                , nub
                                                )
import           Data.Foldable                  ( find )

-- Generate all unique pairs from a list input
pairs :: Eq b => [b] -> [(b, b)]
pairs xs = [ (x, y) | (x : ys) <- tails $ nub xs, y <- ys ]

-- Generate all unique triplets from a list input
triplets :: Eq c => [c] -> [(c, c, c)]
triplets xs =
  [ (x, y, z) | (x : ys) <- tails $ nub xs, (y : zs) <- tails ys, z <- zs ]

-- Optimised for the particular task, far fewer candidates generated
triplets' :: (Eq c, Num c) => (c -> Bool) -> [c] -> [(c, c, c)]
triplets' invariant xs =
  [ (x, y, z)
  | (x : ys) <- tails $ filter invariant $ nub xs
  , (y : zs) <- tails ys
  , invariant $ x + y
  , z <- zs
  ]


findPairWithSum :: (Foldable t, Eq a, Num a) => a -> t (a, a) -> Maybe (a, a)
findPairWithSum targetSum = find ((== targetSum) . (uncurry (+)))

findTripletWithSum
  :: (Foldable t, Eq a, Num a) => a -> t (a, a, a) -> Maybe (a, a, a)
findTripletWithSum targetSum =
  find ((== targetSum) . (\(x, y, z) -> x + y + z))

loadInput :: String -> IO [Int]
loadInput fileName = map read . lines <$> readFile ("src/" ++ fileName)
