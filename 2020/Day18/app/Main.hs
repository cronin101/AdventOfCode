module Main where

import           Lib                            ( loadInput
                                                , sumTotals
                                                , sumTotals2
                                                )

main :: IO ()
main = do
  input <- loadInput "input18.txt"
  print $ sumTotals input
  print $ sumTotals2 input
