{-# LANGUAGE Strict, UnboxedTuples #-}

module Lib
  ( loadInput
  , playGame
  , spokenNumbers
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                     ( fromJust )
import qualified Data.IntMap.Strict            as M
import           Data.List                      ( iterate'
                                                , scanl'
                                                )

-- (Round, LastSaid, LastOccurrences)
type GameState = (Int, Int, M.IntMap Int)

step :: GameState -> GameState
step (round, lastSaid, lastOccurrences) =
  (round + 1, nextSaid, lastOccurrences')
 where
  nextSaid = case M.lookup lastSaid lastOccurrences of
    Just previousRound -> round - previousRound
    _                  -> 0
  lastOccurrences' = M.insert lastSaid round lastOccurrences

playGame :: [Int] -> [GameState]
playGame preamble = init preambleStates ++ iterate' step (last preambleStates)
 where
  preambleStates = scanl'
    (\(_, _, occurrences) (seen, r) -> (r, seen, M.insert seen r occurrences))
    (firstSaid, 1, M.fromList [first])
    xs
  (first@(firstSaid, 1) : xs) = zip preamble [1 ..]

spokenNumbers :: [Int] -> [Int]
spokenNumbers preamble = [ spoken | (_, spoken, _) <- playGame preamble ]

loadInput :: String -> IO [Int]
loadInput fileName =
  map (fst . fromJust . BSC.readInt) . BSC.split ',' <$> BSC.readFile
    ("src/" ++ fileName)
