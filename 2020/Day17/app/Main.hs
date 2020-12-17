module Main where

import           Lib                            ( loadInput
                                                , activeCount
                                                , stepUntil
                                                , Space
                                                  ( ThreeDimensions
                                                  , FourDimensions
                                                  )
                                                )
import           Control.Arrow                  ( Arrow((***)) )
import           Control.Monad                  ( join )

main :: IO ()
main = do
  let load d = loadInput d "input17.txt"
  input3d <- load ThreeDimensions
  input4d <- load FourDimensions

  let (booted3d, booted4d) = join (***) (stepUntil 6) (input3d, input4d)

  print $ activeCount booted3d
  print $ activeCount booted4d

