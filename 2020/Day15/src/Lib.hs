{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , initialState
  , advanceToRound
  , lastSeen
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                     ( fromJust )
import qualified Data.Array.IO                 as IA

type GameState = (Int, Int, IA.IOUArray Int Int)

lastSeen :: (a, b, c) -> b
lastSeen (_, last, _) = last

updateState :: GameState -> Int -> IO GameState
updateState (round, lastSeen, occurrences) nextSeen = do
  IA.writeArray occurrences lastSeen (round - 1)
  return (round + 1, nextSeen, occurrences)

step :: GameState -> IO GameState
step state@(round, lastSaid, occurrences) = do
  let lastRound = round - 1
  lastOccurrence <- IA.readArray occurrences lastSaid
  let nextSaid = if lastOccurrence == -1 then 0 else lastRound - lastOccurrence
  updateState state nextSaid

advanceToRound :: Int -> GameState -> IO GameState
advanceToRound targetRound state@(currentRound, _, _)
  | currentRound >= targetRound = return state
  | otherwise = do
    nextState <- step state
    advanceToRound targetRound nextState

initialState :: Int -> [Int] -> IO GameState
initialState upperBound preamble = do
  newArray <- IA.newArray (0, upperBound) (-1)
  mapM_ (uncurry (IA.writeArray newArray)) (zip (init preamble) [1 ..])
  return (length preamble + 1, last preamble, newArray)

loadInput :: String -> IO [Int]
loadInput fileName =
  map (fst . fromJust . BSC.readInt) . BSC.split ',' <$> BSC.readFile
    ("src/" ++ fileName)
