
module Main (main) where

import           Lib

main :: IO ()
main = do
    input <- loadInput "input.txt"
    let marker = advanceToMarker 4  input
    print $ length . processedReversed $ marker
    let message = advanceToMarker 14  input
    print $ length . processedReversed $ message



