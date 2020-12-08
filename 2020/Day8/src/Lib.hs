{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput
  , detectLoopStart
  , scanProgram
  , executeProgram
  , indexFromState
  , ProgramState(Running, Complete)
  , findIndexToFix
  , toggleAtIndex
  ) where

import           Data.Attoparsec.ByteString.Char8
                                                ( choice
                                                , string
                                                , sepBy1
                                                , space
                                                , Parser
                                                , parseOnly
                                                , anyChar
                                                , endOfLine
                                                , takeWhile1
                                                , isDigit
                                                )
import qualified Data.ByteString.Char8         as BSC
import qualified Data.Array                    as A
import           Data.Array                     ( Array )
import           Data.List                      ( find
                                                , findIndices
                                                )

data Instruction = NOP Int | Jump Int | Accumulate Int
  deriving (Show)

type Index = Int
type Accumulator = Int
data ProgramState = Running (Index, Accumulator) | Complete Accumulator | SegFault
  deriving (Show)
type Program = Array Int Instruction

parseArgument :: Parser Int
parseArgument = do
  sign        <- anyChar
  valueString <- takeWhile1 isDigit
  let value = read . BSC.unpack $ valueString
  return $ if sign == '+' then value else negate value

parseNOP :: Parser Instruction
parseNOP = do
  string "nop"
  space
  NOP <$> parseArgument

parseJump :: Parser Instruction
parseJump = do
  string "jmp"
  space
  Jump <$> parseArgument

parseAccumulate :: Parser Instruction
parseAccumulate = do
  string "acc"
  space
  Accumulate <$> parseArgument

parseInstruction :: Parser Instruction
parseInstruction = choice [parseNOP, parseJump, parseAccumulate]

parseInstructions :: Parser [Instruction]
parseInstructions = sepBy1 parseInstruction endOfLine

step :: Program -> ProgramState -> ProgramState
step _ completed@(Complete _) = completed
step program (Running (index, accumulator)) = case program A.! index of
  NOP        _      -> boundsCheck (index + 1, accumulator)
  Accumulate value  -> boundsCheck (index + 1, accumulator + value)
  Jump       offset -> boundsCheck (index + offset, accumulator)
 where
  (_, lastIndex) = A.bounds program
  boundsCheck (index, value) | index == lastIndex + 1 = Complete value
                             | index > lastIndex      = SegFault
                             | otherwise              = Running (index, value)

scanProgramFrom :: Program -> ProgramState -> [ProgramState]
scanProgramFrom program state = drop 1 $ iterate (step program) state

scanProgram :: Program -> [ProgramState]
scanProgram program = scanProgramFrom program $ Running (0, 0)

isRunning :: ProgramState -> Bool
isRunning (Running _) = True
isRunning _           = False

executeProgram :: Program -> ProgramState
executeProgram = head . dropWhile isRunning . scanProgram

skipOddElements :: [a] -> [a]
skipOddElements xs =
  [ x | (x, evenElement) <- zip xs $ cycle [False, True], evenElement ]

executionIndices :: Program -> [Int]
executionIndices = executionIndicesFrom 0

indexFromState :: ProgramState -> Index
indexFromState (Running (index, _)) = index

executionIndicesFrom :: Int -> Program -> [Int]
executionIndicesFrom index program =
  map indexFromState $ takeWhile isRunning $ scanProgramFrom
    program
    (Running (index, 0))

-- Tortoise and Hare algorithm: First find a collision between standard and double-time pointer chasing
tortoiseHairCollisionIndex :: Program -> Maybe Int
tortoiseHairCollisionIndex program = fmap fst $ find (uncurry (==)) $ zip
  (executionIndices program)
  (skipOddElements $ executionIndices program)

-- Finds the first part of a program where it is looping
detectLoopStart :: Program -> Maybe Int
detectLoopStart program = case tortoiseHairCollisionIndex program of
  Nothing             -> Nothing
  Just collisionIndex -> fmap fst $ find (uncurry (==)) $ zip
    (executionIndices program)
    (executionIndicesFrom collisionIndex program)

programHalts :: Program -> Bool
programHalts program = case tortoiseHairCollisionIndex program of
  Nothing -> True
  Just _  -> False

canToggleJump :: Instruction -> Bool
canToggleJump (Accumulate _) = False
canToggleJump _              = True

toggleJump :: Instruction -> Instruction
toggleJump accumulate@(Accumulate _     ) = accumulate
toggleJump (           Jump       offset) = NOP offset
toggleJump (           NOP        offset) = Jump offset

toggleAtIndex :: Program -> Int -> Program
toggleAtIndex program index =
  program A.// [(index, toggleJump (program A.! index))]

findIndexToFix :: Program -> Maybe Int
findIndexToFix program = find (programHalts . toggleAtIndex program)
                              toggleableIndices
  where toggleableIndices = findIndices canToggleJump $ A.elems program

loadInput :: String -> IO Program
loadInput fileName = do
  parsed <- parseOnly parseInstructions <$> BSC.readFile ("src/" ++ fileName)
  return $ case parsed of
    Right instructions -> A.listArray (0, length instructions - 1) instructions
