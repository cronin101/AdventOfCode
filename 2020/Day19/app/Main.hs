{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                            ( loadInput
                                                , filterValidData
                                                )

main :: IO ()
main = do
  input1 <- loadInput "input19.txt"
  print $ length $ filterValidData input1

  input2 <- loadInput "example2-patched.txt"
  print $ length $ filterValidData input2

