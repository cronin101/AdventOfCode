module Main where

import           Lib                            ( loadInput
                                                , stableState
                                                , passengerCount
                                                )

main :: IO ()
main = do
  emptyFerry <- loadInput "input11.txt"
  print $ passengerCount $ stableState $ emptyFerry
