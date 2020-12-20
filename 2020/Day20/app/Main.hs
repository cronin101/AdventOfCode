module Main where

import           Lib                            ( loadInput
                                                , step
                                                , solve
                                                , multiplyCorners
                                                )

main :: IO ()
main = do
  input <- loadInput "example.txt"

  let (boardState, _) = solve input

  print $ multiplyCorners boardState
