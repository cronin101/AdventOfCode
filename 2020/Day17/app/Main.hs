module Main where

import           Lib                            ( loadInput
                                                , activeCount
                                                , stepUntil
                                                )

main :: IO ()
main = do
  input <- loadInput "input17.txt"
  let booted = stepUntil 6 input
  print $ activeCount booted
