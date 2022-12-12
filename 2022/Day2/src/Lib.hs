{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInputAsStrategyAEntries,
  loadInputAsStrategyBEntries,
  evaluateStrategy,
  scoreRound
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BSC
import           Data.Either                      (fromRight)

data Janken = Rock | Paper | Scissors
  deriving (Show, Eq, Ord, Enum)

data Outcome = Lose | Draw | Win
  deriving (Show)

type StrategyAEntry = (Janken, Janken)

type StrategyBEntry = (Janken, Outcome)

-- >>> evaluateStrategy (Paper, Win)
-- (Paper,Scissors)
evaluateStrategy :: StrategyBEntry -> StrategyAEntry
evaluateStrategy (choice, Draw)   = (choice, choice)
evaluateStrategy (Rock, Win)      = (Rock, Paper)
evaluateStrategy (Rock, Lose)     = (Rock, Scissors)
evaluateStrategy (Paper, Win)     = (Paper, Scissors)
evaluateStrategy (Paper, Lose)    = (Paper, Rock)
evaluateStrategy (Scissors, Win)  = (Scissors, Rock)
evaluateStrategy (Scissors, Lose) = (Scissors, Paper)

-- >>> scoreRound (Rock, Paper)
-- 8
scoreRound :: StrategyAEntry -> Int
scoreRound r@(_, myChoice) = outcomeScore r + shapeScore myChoice

-- >>> outcomeScore (Rock, Paper)
-- 6
outcomeScore :: StrategyAEntry -> Int
outcomeScore (Rock, Paper) = 6
outcomeScore (Paper, Scissors) = 6
outcomeScore (Scissors, Rock) = 6
outcomeScore (hisChoice, myChoice)
  | myChoice == hisChoice = 3
  | otherwise = 0

-- >>> shapeScore Paper
-- 2
shapeScore :: Janken -> Int
shapeScore = (1 +) . fromEnum

-- >>> A.parseOnly parseHisChoice "A"
-- Right Rock
parseHisChoice :: A.Parser Janken
parseHisChoice =
  A.choice
    [ "A" >> return Rock,
      "B" >> return Paper,
      "C" >> return Scissors
    ]

parseMyChoice :: A.Parser Janken
parseMyChoice =
  A.choice
    [ "X" >> return Rock,
      "Y" >> return Paper,
      "Z" >> return Scissors
    ]

parseOutcome :: A.Parser Outcome
parseOutcome = A.choice [
  "X" >> return Lose,
  "Y" >> return Draw,
  "Z" >> return Win
  ]

-- >>> A.parseOnly parseStrategyAEntry "B Z"
-- Right (Paper,Scissors)
parseStrategyAEntry :: A.Parser StrategyAEntry
parseStrategyAEntry = do
  hisChoice <- parseHisChoice <* " "
  myChoice <- parseMyChoice
  return (hisChoice, myChoice)

-- >>> A.parseOnly parseStrategyBEntry "B Z"
-- Right (Paper,Win)
parseStrategyBEntry :: A.Parser StrategyBEntry
parseStrategyBEntry = do
  hisChoice <- parseHisChoice <* " "
  outcome <- parseOutcome
  return (hisChoice, outcome)

parseStrategyAEntries :: A.Parser [StrategyAEntry]
parseStrategyAEntries = A.sepBy1 parseStrategyAEntry A.endOfLine

parseStrategyBEntries :: A.Parser [StrategyBEntry]
parseStrategyBEntries = A.sepBy1 parseStrategyBEntry A.endOfLine

loadInputWithStrategyParser :: A.Parser [a] -> [Char] -> IO [a]
loadInputWithStrategyParser parser fileName =
  fromRight [] . A.parseOnly parser <$> BSC.readFile ("src/" ++ fileName)

loadInputAsStrategyAEntries :: [Char] -> IO [StrategyAEntry]
loadInputAsStrategyAEntries = loadInputWithStrategyParser parseStrategyAEntries

loadInputAsStrategyBEntries :: [Char] -> IO [StrategyBEntry]
loadInputAsStrategyBEntries = loadInputWithStrategyParser parseStrategyBEntries
