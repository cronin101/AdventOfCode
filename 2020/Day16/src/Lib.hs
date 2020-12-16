{-# LANGUAGE Strict, OverloadedStrings #-}
module Lib
  ( loadInput
  , calculateTicketScanningErrorRate
  , solveTicket
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Attoparsec.ByteString.Char8
                                                ( parseOnly
                                                , char
                                                , decimal
                                                , endOfLine
                                                , sepBy1
                                                , string
                                                , Parser
                                                , takeWhile1
                                                , isDigit
                                                )
import           Data.List                      ( transpose )
import qualified Data.Set                      as S

type Range = (Int, Int)
type Ranges = (Range, Range)
type FieldWithConstraint = (BSC.ByteString, Ranges)

type Ticket = [Int]

type Input = ([FieldWithConstraint], Ticket, [Ticket])

data ColumnState = Solved BSC.ByteString | Possibilities (S.Set BSC.ByteString)
    deriving (Show, Eq, Ord)

parseRange :: Parser Range
parseRange = do
  lowString <- takeWhile1 isDigit
  let Just (low, _) = BSC.readInt lowString
  string "-"
  highString <- takeWhile1 isDigit
  let Just (high, _) = BSC.readInt highString
  return (low, high)

parseRanges :: Parser Ranges
parseRanges = do
  r1 <- parseRange
  string " or "
  r2 <- parseRange
  return (r1, r2)

parseFieldWithConstraint :: Parser FieldWithConstraint
parseFieldWithConstraint = do
  name <- takeWhile1 (/= ':')
  string ": "
  ranges <- parseRanges
  return (name, ranges)

parseFieldsWithConstraints :: Parser [FieldWithConstraint]
parseFieldsWithConstraints = sepBy1 parseFieldWithConstraint endOfLine

parseTicket :: Parser Ticket
parseTicket = sepBy1 decimal (char ',')

parseInput :: Parser Input
parseInput = do
  fieldsWithConstraints <- parseFieldsWithConstraints
  endOfLine
  endOfLine
  string "your ticket:\n"
  ticket <- parseTicket
  endOfLine
  endOfLine
  string "nearby tickets:\n"
  tickets <- sepBy1 parseTicket endOfLine
  return (fieldsWithConstraints, ticket, tickets)

validTickets :: Input -> [Ticket]
validTickets (fieldsWithConstraints, _, tickets) =
  filter (validTicket fieldsWithConstraints) tickets

validTicket :: [FieldWithConstraint] -> Ticket -> Bool
validTicket fieldsWithConstraints =
  not . any (fieldIsInvalid fieldsWithConstraints)

fieldSatisfiesConstraint :: Int -> FieldWithConstraint -> Bool
fieldSatisfiesConstraint field (_, ((low1, high1), (low2, high2))) =
  (field >= low1 && field <= high1) || (field >= low2 && field <= high2)

fieldIsInvalid :: [FieldWithConstraint] -> Int -> Bool
fieldIsInvalid fieldsWithConstraints field =
  (not . any (fieldSatisfiesConstraint field)) fieldsWithConstraints

calculateTicketScanningErrorRate :: Input -> Int
calculateTicketScanningErrorRate (fieldsWithConstraints, _, tickets) = sum
  invalidValues
 where
  invalidValues =
    concatMap (filter (fieldIsInvalid fieldsWithConstraints)) tickets

columns :: [Ticket] -> [[Int]]
columns = transpose

possibleFields :: [Int] -> [FieldWithConstraint] -> ColumnState
possibleFields xs fields = case possibleFields of
  [(field, _)] -> Solved field
  _            -> Possibilities (S.fromList $ map fst possibleFields)
 where
  possibleFields =
    filter (\field -> all (`fieldSatisfiesConstraint` field) xs) fields

solveColumns :: [ColumnState] -> [ColumnState]
solveColumns columns | all solved columns = columns
                     | otherwise          = solveColumns columns'
 where
  solvedSet' =
    S.fromList $ map (\(Solved n) -> n) $ filter solved columnsMarkedSolved
  columnsMarkedSolved = map markSolved columns
  columns'            = map filterImpossibilities columnsMarkedSolved
  solved (Solved _) = True
  solved _          = False
  markSolved c@(Possibilities p) | S.size p == 1 = Solved (head $ S.elems p)
                                 | otherwise     = c
  markSolved c = c
  filterImpossibilities c@(Solved _) = c
  filterImpossibilities (Possibilities ps) = Possibilities (ps S.\\ solvedSet')

solveTicket
  :: ([FieldWithConstraint], [Int], [Ticket]) -> [(BSC.ByteString, Int)]
solveTicket input@(fields, ticket, _) = zip solvedColumns ticket
 where
  onlyValidTickets = validTickets input
  columnPossibilities =
    map (`possibleFields` fields) $ columns onlyValidTickets
  solvedColumns = map (\(Solved bs) -> bs) $ solveColumns columnPossibilities

loadInput :: String -> IO Input
loadInput fileName = do
  Right input <- parseOnly parseInput <$> BSC.readFile ("src/" ++ fileName)
  return input
