module Main where

import           Lib                            ( loadInput
                                                , step
                                                )

main :: IO ()
main = do
  input <- loadInput "example.txt"

  let (board, _) = step $ step input
  print board
