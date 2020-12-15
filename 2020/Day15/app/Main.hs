module Main where

import           Lib                            ( loadInput
                                                , initialState
                                                , advanceToRound
                                                , lastSeen
                                                )

main :: IO ()
main = do
  input <- loadInput "input15.txt"
  let limit = 30000000
  initialState <- initialState limit input

  partOne      <- advanceToRound 2021 initialState
  print $ lastSeen partOne

  partTwo <- advanceToRound (limit + 1) partOne
  print $ lastSeen partTwo
