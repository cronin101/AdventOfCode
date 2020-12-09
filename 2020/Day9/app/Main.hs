module Main where

import           Lib                            ( loadInput
                                                , initialiseStream
                                                , checkHead
                                                , playStream
                                                , findSegmentWithValue
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( find )

main :: IO ()
main = do
  input <- loadInput "input9.txt"
  let preambleLength    = 25
  let stream = initialiseStream preambleLength input
  let checkedStream = mapMaybe checkHead $ playStream stream
  let Just firstInvalid = fst <$> find ((== False) . snd) checkedStream
  print firstInvalid

  let segmentWithValue = findSegmentWithValue input firstInvalid
  let segmentMin       = minimum segmentWithValue
  let segmentMax       = maximum segmentWithValue
  print $ segmentMin + segmentMax
