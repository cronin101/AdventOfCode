module Main (main) where

import           Lib

main :: IO ()
main = do
    input <- loadInput "input.txt"
    let invalidPairs = filter (uncurry hasContainedRange) input
    print $ length invalidPairs
    let overlappingPairs = filter (uncurry overlaps) input
    print $ length overlappingPairs
