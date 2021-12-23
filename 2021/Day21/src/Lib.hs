{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

module Lib (loadInput, toQuantumState, playQuantum, mostWins, computeOutput, playUntilWinner) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (Bifunctor (bimap, first, second))
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.List (partition, sortBy)
import qualified Data.Map as M

-- (Score, Position)
type QuantumPlayerState = (Int, Int)

-- (Player1, Player2, IsP1Next)
type QuantumState = (QuantumPlayerState, QuantumPlayerState, Bool)

type QuantumStates = M.Map QuantumState Int

data GameState = GameState
  { scores :: (Int, Int),
    pastRolls :: [Int],
    futureRolls :: [Int],
    spaces :: ([Int], [Int]),
    spaceAccessors :: [(([Int] -> [Int]) -> ([Int], [Int]) -> ([Int], [Int]), ([Int], [Int]) -> [Int])],
    scoreAccessors :: [((Int -> Int) -> (Int, Int) -> (Int, Int), (Int, Int) -> Int)]
  }

instance Show GameState where
  show g = "Scores: " ++ show (scores g) ++ ", Positions: " ++ show (bimap head head (spaces g)) ++ ", Roll-count: " ++ show (length $ pastRolls g)

initialState :: (Int, Int) -> GameState
initialState (p1Start, p2Start) =
  GameState
    { scores = (0, 0),
      pastRolls = [],
      futureRolls = cycle [1 .. 100],
      spaces = (dropWhile (/= p1Start) $ cycle [1 .. 10], dropWhile (/= p2Start) $ cycle [1 .. 10]),
      scoreAccessors = cycle [(first, fst), (second, snd)],
      spaceAccessors = cycle [(first, fst), (second, snd)]
    }

-- >>> A.parseOnly parseInitialState "Player 1 starting position: 4\nPlayer 2 starting position: 8"
-- Right Scores: (0,0), Positions: (4,8)
parseInitialState :: A.Parser GameState
parseInitialState = do
  "Player 1 starting position: "
  p1Start <- A.decimal
  A.endOfLine
  "Player 2 starting position: "
  p2Start <- A.decimal
  return $ initialState (p1Start, p2Start)

-- >>> loadInput "input.txt"
-- Scores: (0,0), Positions: (6,9), Roll-count: 0
loadInput :: [Char] -> IO GameState
loadInput fileName =
  fromRight (initialState (0, 0))
    . A.parseOnly parseInitialState
    <$> BSC.readFile
      ("src/" ++ fileName)

step :: GameState -> GameState
step (GameState scores pastRolls futureRolls spaces spaceAccessors scoreAccessors) = GameState scores' pastRolls' futureRolls' spaces' spaceAccessors' scoreAccessors'
  where
    (rolls, futureRolls') = splitAt 3 futureRolls
    pastRolls' = rolls ++ pastRolls
    ((writeSpaces, readSpaces), spaceAccessors') = first head $ splitAt 1 spaceAccessors
    ((writeScore, readScore), scoreAccessors') = first head $ splitAt 1 scoreAccessors
    spaces' = writeSpaces (drop $ sum rolls) spaces
    scores' = writeScore (+ (head $ readSpaces spaces')) scores

playUntilWinner :: GameState -> GameState
playUntilWinner = head . dropWhile ((< 1000) . uncurry max . scores) . iterate step

-- >>> step $ initialState (4, 8)
-- Scores: (10,0), Positions: (10,8), Roll-count: 3

-- >>> playUntilWinner $ initialState (4, 8)
-- Scores: (1000,745), Positions: (10,3), Roll-count: 993

computeOutput :: GameState -> Int
computeOutput g = uncurry min (scores g) * length (pastRolls g)

-- >>> computeOutput $ playUntilWinner $ initialState (4, 8)
-- 739785

-- >>> computeOutput . playUntilWinner <$> loadInput "input.txt"
-- 925605

-- >>> toQuantumState $ initialState (4, 8)
-- fromList [(((0,4),(0,8),True),1)]
toQuantumState :: GameState -> QuantumStates
toQuantumState g = M.singleton ((fst $ scores g, head $ fst $ spaces g), (snd $ scores g, head $ snd $ spaces g), True) 1

playQuantum :: QuantumStates -> QuantumStates
playQuantum = (until =<< ((==) =<<)) quantumStep

quantumStep :: QuantumStates -> QuantumStates
quantumStep states
  | null running = states
  | otherwise = M.unionWith (+) endedStates newStates
  where
    endedStates = M.unionsWith (+) $ map (\state -> M.singleton state (states M.! state)) ended
    newStates = M.unionsWith (+) $ map (M.unionsWith (+) . map (uncurry M.singleton) . nextStates . (\count -> (count, states M.! count))) running
    (ended, running) = partition (\((score1, _), (score2, _), _) -> max score1 score2 >= 21) $ M.keys states
    nextStates :: (QuantumState, Int) -> [(QuantumState, Int)]
    nextStates (state, count) =
      [ ((,count) . nextState state) (x + y + z)
        | x <- [1 .. 3],
          y <- [1 .. 3],
          z <- [1 .. 3]
      ]
    nextState :: QuantumState -> Int -> QuantumState
    nextState ((score1, space1), (score2, space2), isP1Next) roll =
      if isP1Next
        then
          let space1' = head $ drop roll $ dropWhile (/= space1) $ cycle [1 .. 10]
           in ((score1 + space1', space1'), (score2, space2), not isP1Next)
        else
          let space2' = head $ drop roll $ dropWhile (/= space2) $ cycle [1 .. 10]
           in ((score1, space1), (score2 + space2', space2'), not isP1Next)

-- >>> mostWins $ playQuantum $ toQuantumState $ initialState (6, 9)
-- 486638407378784
mostWins :: QuantumStates -> Int
mostWins states = highestCount
  where
    highestCount = uncurry max (getTotal p1Wins, getTotal p2Wins)
    getTotal = sum . map snd
    (p1Wins, p2Wins) = partition (\(((p1Score, _), (p2Score, _), _), _) -> p1Score > p2Score) $ M.toList states
