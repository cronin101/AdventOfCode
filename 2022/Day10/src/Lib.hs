{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    runProgram,
    signalStrengthSum,
    printExecution,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.IntMap qualified as IM
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import GHC.Base ((<|>))

data Instruction = NoOp | AddX Int
  deriving (Show)

type Registers = M.Map Char Int

data RegisterState = RegisterState {current :: Registers, next :: Registers}
  deriving (Show)

type Execution = IM.IntMap RegisterState

type ExecutionCycle = Registers -> Registers

parseNoOp :: A.Parser Instruction
parseNoOp = NoOp <$ "noop"

-- >>> A.parseOnly parseAddX "addx 3"
-- Right (AddX 3)
-- >>> A.parseOnly parseAddX "addx -5"
-- Right (AddX (-5))
parseAddX :: A.Parser Instruction
parseAddX = AddX <$> ("addx " *> A.signed A.decimal)

parseInstructions :: A.Parser [Instruction]
parseInstructions = A.sepBy1 (parseNoOp <|> parseAddX) A.endOfLine

loadInput :: String -> IO [Instruction]
loadInput = (fromRight [] . A.parseOnly parseInstructions <$>) . BSC.readFile . ("src/" ++)

execute :: Instruction -> [ExecutionCycle]
execute NoOp = pure id
execute (AddX x) = [id, M.insertWith (+) 'x' x]

initialRegisters :: RegisterState
initialRegisters =
  RegisterState
    { current = intialRegister,
      next = intialRegister
    }
  where
    intialRegister = M.singleton 'x' 1

runProgram :: [Instruction] -> Execution
runProgram = IM.fromList . zip [0 ..] . scanl (flip applyToR) initialRegisters . concatMap execute
  where
    applyToR f (RegisterState _ n) = RegisterState n (f n)

signalStrength :: Execution -> Int -> Int
signalStrength execution cyc = cyc * M.findWithDefault 0 'x' (current $ execution IM.! cyc)

signalStrengthSum :: Execution -> Int
signalStrengthSum e = sum $ map (e `signalStrength`) [20, 60, 100, 140, 180, 220]

pixels :: [[Int]]
pixels = chunksOf 40 [1 .. 240]

toBeam :: Execution -> Int -> String
toBeam e cyc
  | xPosition + signum (pixelPosition - xPosition) == pixelPosition = "█"
  | otherwise = " "
  where
    xPosition = (cyc - 1) `mod` 40
    pixelPosition = fromJust $ M.lookup 'x' $ current $ fromJust $ IM.lookup cyc e

printExecution :: Execution -> String
printExecution e = intercalate "\n" $ map (concatMap (toBeam e)) pixels