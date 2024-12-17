{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    explain,
    part2,
  )
where

import Control.Monad.Writer qualified as W
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bits (Bits (shiftL, shiftR), xor)
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (intercalate, intersperse)
import Data.Map qualified as M
import Data.Vector qualified as V

type Registers = M.Map Char Int

data Machine = Machine
  { pc' :: Int,
    output' :: [Int],
    registers' :: Registers,
    instructions' :: V.Vector Int
  }
  deriving (Show)

data Log where
  Log :: String -> Log

instance Show Log where
  show :: Log -> String
  show (Log s) = s

-- >>> A.parseOnly parseRegister "Register A: 729"
-- Right (fromList [('A',729)])
parseRegister :: A.Parser Registers
parseRegister = M.singleton <$> ("Register " *> A.anyChar <* ": ") <*> A.decimal

parseRegisters :: A.Parser Registers
parseRegisters = M.unions <$> parseRegister `A.sepBy` A.endOfLine

-- >>> A.parseOnly parseProgram "Program: 0,1,5,4,3,0"
-- Right [0,1,5,4,3,0]
parseProgram :: A.Parser (V.Vector Int)
parseProgram = V.fromList <$> ("Program: " *> A.decimal `A.sepBy` ",")

parseMachine :: A.Parser Machine
parseMachine = Machine 0 [] <$> (parseRegisters <* A.count 2 A.endOfLine) <*> parseProgram

step :: Machine -> W.Writer [String] Machine
step m@(Machine pc output registers instructions) = perform (V.toList $ V.slice pc 2 instructions)
  where
    load :: Char -> W.Writer [String] Int
    load r = do
      let v = registers M.! r
      W.tell ["Loaded value " ++ show v ++ " from register " ++ [r]]
      return $ registers M.! r
    store :: Char -> Int -> W.Writer [String] Registers
    store r v = do
      W.tell ["Storing value " ++ show v ++ " in register " ++ [r]]
      let registers'' = M.insert r v registers
      W.tell ["Registers: " ++ show registers'']
      return registers''
    combo :: Int -> W.Writer [String] Int
    combo 6 = do
      W.tell ["Combo: Register C"]
      load 'C'
    combo 5 = do
      W.tell ["Combo: Register B"]
      load 'B'
    combo 4 = do
      W.tell ["Combo: Register A"]
      load 'A'
    combo n = do
      W.tell ["Combo: Literal " ++ show n]
      return n
    next = m {pc' = pc + 2}
    -- adv: Division of A into A
    perform [0, c] = do
      W.tell ["adv"]
      let r = 'A'
      n <- load r
      power <- combo c
      let d = 2 ^ power
      W.tell ["Dividing A by 2 raised to" ++ show power]
      let v = floor $ fromIntegral n / d
      registers'' <- store r v
      return $ next {registers' = registers''}
    -- bxl: Bitwise XOR B with literal
    perform [1, l] = do
      W.tell ["bxl"]
      let r = 'B'
      n <- load r
      W.tell ["Taking bitwise XOR of register B and literal " ++ show l]
      let v = n `xor` l
      registers'' <- store r v
      return next {registers' = registers''}
    -- bst: Combo modulo 8
    perform [2, c] = do
      W.tell ["bst"]
      let r = 'B'
      n <- combo c
      W.tell ["Taking modulo 8 of value " ++ show n]
      let v = n `mod` 8
      registers'' <- store r v
      return next {registers' = registers''}
    -- jnz: Jump if A is not zero
    perform [3, l]
      | registers M.! 'A' == 0 = do
          W.tell ["jnz: nop because register A is zero. Would have gone to " ++ show l]
          return next
      | otherwise = do
          W.tell ["jnz: Jump since register A is not zero. Going to " ++ show l]
          return next {pc' = l}
    -- bxc: Bitwise XOR B with C
    perform [4, _] = do
      W.tell ["bxc"]
      W.tell ["Taking bitwise XOR of register B and C, storing in B"]
      b <- load 'B'
      c <- load 'C'
      registers'' <- store 'B' (b `xor` c)
      return next {registers' = registers''}
    -- out: Output
    perform [5, c] = do
      W.tell ["out"]
      literal <- combo c
      W.tell ["Taking modulo 8 of " ++ show literal]
      W.tell ["Outputting " ++ show (literal `mod` 8)]
      return next {output' = literal `mod` 8 : output}
    -- bdv: Division of A into B
    perform [6, c] = do
      W.tell ["bdv"]
      n <- load 'A'
      power <- combo c
      let d = 2 ^ power
      W.tell ["Dividing A by 2 raised to " ++ show power]
      let v = floor $ fromIntegral n / d
      registers'' <- store 'B' v
      return next {registers' = registers''}
    -- cdv: Division of A into C
    perform [7, c] = do
      W.tell ["cdv"]
      n <- load 'A'
      power <- combo c
      let d = 2 ^ power
      W.tell ["Dividing A by 2 raised to " ++ show power]
      let v = floor $ fromIntegral n / d
      registers'' <- store 'C' v
      return next {registers' = registers''}
    -- Error
    perform _ = error "Invalid instruction"

-- >>> run <$> loadInput "example.txt"
-- Machine {pc' = 6, output = [0,1,2,5,3,6,5,3,6,4], registers' = fromList [('A',0),('B',0),('C',0)], instructions' = [0,1,5,4,3,0]}
run :: Machine -> W.Writer [String] Machine
run m
  | pc' m == V.length (instructions' m) = do
      W.tell ["Completed with output " ++ show (extractOutput m)]
      return m
  | otherwise = do
      W.tell ["{"]
      m' <- step m
      W.tell ["}"]
      run m'

-- >>> loadInput "example.txt"
-- Machine {clock' = 0, pc' = 0, output' = [], registers' = fromList [('A',729),('B',0),('C',0)], instructions' = [0,1,5,4,3,0]}
loadInput :: [Char] -> IO Machine
loadInput = (fromRight (Machine 0 [] M.empty V.empty) . A.parseOnly parseMachine <$>) . BSC.readFile . ("src/" ++)

extractOutput :: Machine -> [Int]
extractOutput = reverse . output'

-- >>> part1 <$> loadInput "example.txt"
-- "4,6,3,5,6,3,5,2,1,0"
part1 :: Machine -> String
part1 m = intersperse ',' $ map (head . show) $ extractOutput $ fst $ W.runWriter (run m)

-- >>>   explain <$> loadInput "example2.txt"
-- {
-- adv
-- Loaded value 2024 from register A
-- Combo: Literal 3
-- Dividing A by 2 raised to3
-- Storing value 253 in register A
-- Registers: fromList [('A',253),('B',0),('C',0)]
-- }
-- {
-- out
-- Combo: Register A
-- Loaded value 253 from register A
-- Taking modulo 8 of 253
-- Outputting 5
-- }
-- {
-- jnz: Jump since register A is not zero. Going to 0
-- }
-- {
-- adv
-- Loaded value 253 from register A
-- Combo: Literal 3
-- Dividing A by 2 raised to3
-- Storing value 31 in register A
-- Registers: fromList [('A',31),('B',0),('C',0)]
-- }
-- {
-- out
-- Combo: Register A
-- Loaded value 31 from register A
-- Taking modulo 8 of 31
-- Outputting 7
-- }
-- {
-- jnz: Jump since register A is not zero. Going to 0
-- }
-- {
-- adv
-- Loaded value 31 from register A
-- Combo: Literal 3
-- Dividing A by 2 raised to3
-- Storing value 3 in register A
-- Registers: fromList [('A',3),('B',0),('C',0)]
-- }
-- {
-- out
-- Combo: Register A
-- Loaded value 3 from register A
-- Taking modulo 8 of 3
-- Outputting 3
-- }
-- {
-- jnz: Jump since register A is not zero. Going to 0
-- }
-- {
-- adv
-- Loaded value 3 from register A
-- Combo: Literal 3
-- Dividing A by 2 raised to3
-- Storing value 0 in register A
-- Registers: fromList [('A',0),('B',0),('C',0)]
-- }
-- {
-- out
-- Combo: Register A
-- Loaded value 0 from register A
-- Taking modulo 8 of 0
-- Outputting 0
-- }
-- {
-- jnz: nop because register A is zero. Would have gone to 0
-- }
-- Completed with output [5,7,3,0]
explain :: Machine -> Log
explain m = Log $ intercalate "\n" $ W.execWriter (run m)

withNewA :: Machine -> Int -> Machine
withNewA m i = m {registers' = M.insert 'A' i (registers' m)}

-- >>> part2 <$> loadInput "input.txt"
-- 190384113204239
part2 :: Machine -> Int
part2 m = fst $ find (V.toList $ instructions' m) (0, []) m

-- >>> find [2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0] (0, []) <$> loadInput "input.txt"
-- (190384113204239,[2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0])
find :: [Int] -> (Int, [Int]) -> Machine -> (Int, [Int])
find target (base, found) m
  | target == found = (base `shiftR` 3, found)
  | otherwise = case findNextLowestBits base (drop (length target - (1 + length found)) target) m of
      [] -> (base, found)
      (nextBase, nextFound) : _ -> find target (nextBase `shiftL` 3, nextFound) m

-- >>> findNextLowestBits 0 [0] <$> loadInput "input.txt"
-- [(5,[0])]
-- >>> findNextLowestBits (5 `shiftL` 3) [3, 0] <$> loadInput "input.txt"
-- [(43,[3,0]),(47,[3,0])]

-- >>> findNextLowestBits (43 `shiftL` 3) [5, 3, 0] <$> loadInput "input.txt"
-- [(346,[5,3,0])]
findNextLowestBits :: Int -> [Int] -> Machine -> [(Int, [Int])]
findNextLowestBits base target m =
  filter
    ((== target) . snd)
    [(base + i, extractOutput $ fst $ W.runWriter (run $ m `withNewA` (base + i))) | i <- [0 .. 7]]
