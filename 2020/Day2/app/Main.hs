module Main where

import           Lib                            ( loadInput
                                                , evaluateLine
                                                )
import           Data.ByteString                ( ByteString )

answerOne :: [ByteString] -> Int
answerOne input = length . filter fst $ map evaluateLine input

answerTwo :: [ByteString] -> Int
answerTwo input = length . filter snd $ map evaluateLine input

main :: IO ()
main = do
  input <- loadInput "input2.txt"
  print $ answerOne input
  print $ answerTwo input
