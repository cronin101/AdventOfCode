module Main where

import           Lib                            ( loadInput
                                                , scanStates
                                                , sumData
                                                , MachineVersion
                                                  ( VersionOne
                                                  , VersionTwo
                                                  )
                                                )

main :: IO ()
main = do
  instructions <- loadInput "input14.txt"
  print $ sumData . last . scanStates VersionOne $ instructions

  print $ sumData . last . scanStates VersionTwo $ instructions
