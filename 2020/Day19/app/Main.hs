{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                            ( loadInput
                                                , filterValidData
                                                , balancedIngestor
                                                , repeatingIngestor
                                                )
import qualified Data.IntMap.Strict            as M

main :: IO ()
main = do
  input <- loadInput "input19.txt"
  print $ length $ filterValidData input M.empty

  let overrides = M.fromList
        [(11, balancedIngestor (42, 31)), (8, repeatingIngestor 42)]

  print $ length $ filterValidData input overrides
