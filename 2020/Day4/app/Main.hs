module Main where

import           Lib                            ( loadInput
                                                , hasRequiredCredentials
                                                , credentialsAreValid
                                                )

main :: IO ()
main = do
  passengers <- loadInput "input4.txt"
  let passengersWithRequiredCredentials =
        filter hasRequiredCredentials passengers
  print $ length passengersWithRequiredCredentials

  let passengersWithValidCredentials =
        filter credentialsAreValid passengersWithRequiredCredentials
  print $ length passengersWithValidCredentials
