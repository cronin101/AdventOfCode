module Main where

import           Lib                            ( loadInput
                                                , parseLine
                                                )
import           Data.ByteString                ( ByteString )

answerOne :: [ByteString] -> Int
answerOne input = length . filter fst $ map parseLine input

answerTwo :: [ByteString] -> Int
answerTwo input = length . filter snd $ map parseLine input

main :: IO ()
main = do
  input <- loadInput "input2.txt"
  print $ answerOne input
  print $ answerTwo input
