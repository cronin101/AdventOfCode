module Main (main) where

import           Lib

main :: IO ()
main = do
    input <- loadInput "input.txt"
    let duplicates = map duplicateItem input
    print $ sum duplicates
    let badges = getBadges input
    print $ sum badges
