module Main where

import           Lib                            ( pairs
                                                , findPairWithSum
                                                , findTripletWithSum
                                                , loadInput
                                                )

main :: IO ()
main = do
  input <- loadInput "input1.txt"
  let desiredPair = findPairWithSum 2020 $ pairs input
  let pairProduct = uncurry (*) <$> desiredPair
  print pairProduct

  let desiredTriplet = findTripletWithSum 2020 input
  let tripletProduct = fmap (\(x, y, z) -> x * y * z) desiredTriplet
  print tripletProduct
