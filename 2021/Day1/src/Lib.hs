{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    countIncreases,
    countWindowIncreases,
  )
where

import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl', tails, transpose)
import Data.Maybe (mapMaybe)

countIncreases :: Ord a => [a] -> Int
countIncreases xs = length $ filter (uncurry (<)) $ zip xs (drop 1 xs)

countWindowIncreases :: [Int] -> Int
countWindowIncreases = countIncreases . map sum . windowsOfLengthThree

windowsOfLengthThree :: [a] -> [[a]]
windowsOfLengthThree list = take (length list - 2) $ transpose [list, drop 1 list, drop 2 list]

loadInput :: [Char] -> IO [Int]
loadInput fileName =
  map fst . mapMaybe BSC.readInt . BSC.lines
    <$> BSC.readFile
      ("src/" ++ fileName)
