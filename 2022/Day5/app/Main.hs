module Main (main) where

import           Lib

main :: IO ()
main = do
    startState <- loadInput "input.txt"
    let endStateWithoutCrane = run False startState
    print $ crateHeads endStateWithoutCrane
    let endStateWithCrane = run True startState
    print $ crateHeads endStateWithCrane
