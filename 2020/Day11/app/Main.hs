module Main where

import           Lib                            ( loadInput
                                                , stableState
                                                , passengerCount
                                                , PassengerTolerance
                                                  ( PassengerTolerance
                                                  )
                                                , AdjacencyType
                                                  ( Immediate
                                                  , NearestVisible
                                                  )
                                                )

main :: IO ()
main = do
  firstFerry <- loadInput "input11.txt" (PassengerTolerance Immediate 3)
  print $ passengerCount $ stableState firstFerry

  secondFerry <- loadInput "input11.txt" (PassengerTolerance NearestVisible 4)
  print $ passengerCount $ stableState secondFerry
