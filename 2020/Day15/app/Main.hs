module Main where

import           Lib                            ( loadInput
                                                , spokenNumbers
                                                )

main :: IO ()
main = do
  input <- loadInput "input15.txt"
  print input

  print $ spokenNumbers input !! (2020 - 1)
  -- tenth of the work
  print $ spokenNumbers input !! (30000000 - 1)


