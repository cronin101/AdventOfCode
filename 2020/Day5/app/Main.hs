module Main where

import           Lib                            ( loadInput
                                                , calculateSeatId
                                                , missingId
                                                )

main :: IO ()
main = do
  boardingPasses <- loadInput "input5.txt"
  let seatIds = map calculateSeatId boardingPasses
  print $ maximum seatIds

  print $ missingId seatIds
