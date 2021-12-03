module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "input.txt"
  let gammaInt = readBinary $ gamma input
  let epsilonInt = readBinary $ epsilon input
  print $ gammaInt * epsilonInt
  let oxygenRatingInt = readBinary $ oxygenRating input
  let scrubberRatingInt = readBinary $ scrubberRating input
  print $ oxygenRatingInt * scrubberRatingInt