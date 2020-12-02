module Main where

import           Lib                            ( findPairWithSum
                                                , findTripletWithSum
                                                , loadInput
                                                )
import           Data.IntSet                    ( IntSet )

target :: Int
target = 2020

answerOne :: IntSet -> Maybe Int
answerOne input = uncurry (*) <$> findPairWithSum target input


answerTwo :: IntSet -> Maybe Int
answerTwo input = (\(x, y, z) -> x * y * z) <$> findTripletWithSum target input

main :: IO ()
main = do
  input <- loadInput "input1.txt"
  print $ answerOne input
  print $ answerTwo input
