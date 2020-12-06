module Main where

import           Lib                            ( loadInput )
import           Data.IntSet                    ( size
                                                , unions
                                                , intersection
                                                )


main :: IO ()
main = do
  groupedDeclarations <- loadInput "input6.txt"
  let unionDeclarations = map unions groupedDeclarations
  print $ sum $ map size unionDeclarations

  let intersectionDeclarations = map (foldl1 intersection) groupedDeclarations
  print $ sum $ map size intersectionDeclarations
