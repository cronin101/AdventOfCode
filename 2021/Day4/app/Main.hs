{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  input <- loadInput "example.txt"
  print input
