module Main where

import           Lib                            ( loadInput
                                                , scanStates
                                                , initialState
                                                , manhattanDistance
                                                , NavigationStyle
                                                  ( Direct
                                                  , Waypoint
                                                  )
                                                )

main :: IO ()
main = do
  instructions <- loadInput "input12.txt"
  let directStates   = scanStates (initialState Direct) instructions
  let directDistance = manhattanDistance $ last directStates
  print directDistance

  let waypointStates   = scanStates (initialState Waypoint) instructions
  let waypointDistance = manhattanDistance $ last waypointStates
  print waypointDistance
