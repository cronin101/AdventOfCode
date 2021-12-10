{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    syntaxCheck,
    isCorrupted,
    isIncomplete,
    scoreCorruptedSyntaxCheckState,
    scoreIncompleteSyntaxCheckState,
    takeMiddle,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.List (sort)
import qualified Data.Set as S

-- (RemainingString, OpenScopes)
type SyntaxCheckState = (String, String)

-- >>> A.parseOnly parseBrackets "[({(<(())[]>[[{[]{<()<>>"
-- Right "[({(<(())[]>[[{[]{<()<>>"
parseBrackets :: A.Parser String
parseBrackets = A.many1 $ A.choice $ map A.char ['{', '}', '(', ')', '<', '>', '[', ']']

parseInput :: A.Parser [String]
parseInput = A.sepBy1 parseBrackets A.endOfLine

-- >>> loadInput "example.txt"
-- ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]
loadInput :: String -> IO [String]
loadInput fileName =
  fromRight []
    . A.parseOnly parseInput
    <$> BSC.readFile
      ("src/" ++ fileName)

openingBrackets :: S.Set Char
openingBrackets = S.fromList "([<{"

-- >>> syntaxCheck "[({(<(())[]>[[{[]{<()<>>"
-- ("","{{[[({([")
-- >>> syntaxCheck "{([(<{}[<>[]}>{[]{[(<()>"
-- ("}>{[]{[(<()>","[<([({")
syntaxCheck :: String -> SyntaxCheckState
syntaxCheck string = syntaxCheck' (string, "")
  where
    syntaxCheck' :: SyntaxCheckState -> SyntaxCheckState
    syntaxCheck' ([], scopes) = ([], scopes)
    syntaxCheck' (')' : xs, '(' : ss) = syntaxCheck' (xs, ss)
    syntaxCheck' (']' : xs, '[' : ss) = syntaxCheck' (xs, ss)
    syntaxCheck' ('>' : xs, '<' : ss) = syntaxCheck' (xs, ss)
    syntaxCheck' ('}' : xs, '{' : ss) = syntaxCheck' (xs, ss)
    syntaxCheck' (remaining@(x : xs), scopes) = if S.member x openingBrackets then syntaxCheck' (xs, x : scopes) else (remaining, scopes)

-- >>> isCorrupted $ syntaxCheck "[({(<(())[]>[[{[]{<()<>>"
-- False
-- >>> isCorrupted $ syntaxCheck "{([(<{}[<>[]}>{[]{[(<()>"
-- True
isCorrupted :: SyntaxCheckState -> Bool
isCorrupted ([], _) = False
isCorrupted _ = True

-- >>> scoreSyntaxCheckState $ syntaxCheck "{([(<{}[<>[]}>{[]{[(<()>"
-- 1197
scoreCorruptedSyntaxCheckState :: SyntaxCheckState -> Int
scoreCorruptedSyntaxCheckState (')' : _, _) = 3
scoreCorruptedSyntaxCheckState (']' : _, _) = 57
scoreCorruptedSyntaxCheckState ('}' : _, _) = 1197
scoreCorruptedSyntaxCheckState ('>' : _, _) = 25137
scoreCorruptedSyntaxCheckState (_, _) = 0

-- >>> isIncomplete $ syntaxCheck "[({(<(())[]>[[{[]{<()<>>"
-- True
isIncomplete :: SyntaxCheckState -> Bool
isIncomplete ([], []) = False
isIncomplete ([], _) = True
isIncomplete (_, _) = False

-- >>> scoreIncompleteSyntaxCheckState $ syntaxCheck "[({(<(())[]>[[{[]{<()<>>"
-- 288957
scoreIncompleteSyntaxCheckState :: SyntaxCheckState -> Int
scoreIncompleteSyntaxCheckState (_, scopes) = foldl1 ((+) . (5 *)) $ map scoreRemainingScope scopes
  where
    scoreRemainingScope '(' = 1
    scoreRemainingScope '[' = 2
    scoreRemainingScope '{' = 3
    scoreRemainingScope '<' = 4
    scoreRemainingScope _ = 0

-- >>> takeMiddle [3,1,2]
-- 2
takeMiddle :: [Int] -> Int
takeMiddle scores = head . middle $ sort scores
  where
    middle l@(_ : _ : _ : _) = middle $ tail $ init l
    middle l = l
