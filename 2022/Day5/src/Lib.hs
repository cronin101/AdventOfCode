{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( loadInput,
    run, crateHeads
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BSC
import           Data.Either                      (fromRight)
import qualified Data.IntMap                      as M
import           Data.List                        (transpose)
import           Data.Maybe                       (catMaybes, fromJust)

data Instruction = Move { count :: Int, from :: Int, to :: Int}
    deriving (Show)

type State = (M.IntMap [Char], [Instruction])

-- >>> A.parseOnly parseBox "[A]"
-- Right 'A'
parseBox :: A.Parser Char
parseBox = "[" *> A.anyChar <* "]"

-- >>> A.parseOnly parseMaybeBox "[A]"
-- Right (Just 'A')
-- >>> A.parseOnly parseMaybeBox "   "
-- Right Nothing
parseMaybeBox :: A.Parser (Maybe Char)
parseMaybeBox = A.choice [Just <$> parseBox,  Nothing <$ "   "]

-- >>> A.parseOnly parseBoxRow "    [D]    "
-- Right [Nothing,Just 'D',Nothing]
-- >>> A.parseOnly parseBoxRow "[Z] [M] [P]"
-- Right [Just 'Z',Just 'M',Just 'P']
parseBoxRow :: A.Parser [Maybe Char]
parseBoxRow = A.sepBy1 parseMaybeBox " "

parseBoxRows :: A.Parser [[Maybe Char]]
parseBoxRows = A.sepBy1 parseBoxRow A.endOfLine

-- >>> A.parseOnly parseRowLegend " 1   2   3 "
-- Right [1,2,3]
parseRowLegend :: A.Parser [Int]
parseRowLegend = A.sepBy1 (" " *> A.decimal <* " ") " "

parseBoxesWithLegend :: A.Parser (M.IntMap [Char])
parseBoxesWithLegend = do
    rows <- map catMaybes . transpose <$> parseBoxRows <* A.endOfLine
    legend <- parseRowLegend
    return $ M.fromList $ zip legend rows

-- >>> A.parseOnly parseInstruction "move 1 from 2 to 1"
-- Right (Move {count = 1, from = 2, to = 1})
parseInstruction :: A.Parser Instruction
parseInstruction = do
    c <- "move " *> A.decimal
    f <- " from " *> A.decimal
    t <- " to " *> A.decimal
    return $ Move c f t

parseInstructions :: A.Parser [Instruction]
parseInstructions = A.sepBy1 parseInstruction A.endOfLine

parseInput :: A.Parser State
parseInput = do
    boxState <- parseBoxesWithLegend <* A.count 2 A.endOfLine
    instructions <- parseInstructions
    return (boxState, instructions)

step :: Bool -> State -> State
step _ state@(_, []) = state
step useCrane (boxState, Move c f t : is) = (boxState', is)
    where
        boxState' = M.insert t (movedChonk ++ target) $ M.insert f remainder boxState
        target = fromJust $ M.lookup t boxState
        movedChonk = if useCrane then chonk else reverse chonk
        (chonk, remainder) = splitAt c $ fromJust $ M.lookup f boxState

run :: Bool -> State -> State
run useCrane = head . dropWhile (not . null . snd) . iterate (step useCrane)

crateHeads :: State -> String
crateHeads (boxState, _) = map (head . snd) $ M.toAscList boxState

loadInput :: [Char] -> IO State
loadInput fileName =
   fromRight (M.empty, []) . A.parseOnly parseInput <$> BSC.readFile ("src/" ++ fileName)

