{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput
  , filterValidData
  ) where

import qualified Data.ByteString.Char8         as B
import qualified Data.IntSet                   as S
import qualified Data.IntMap                   as M
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.List                      ( partition )
import           Control.Arrow                  ( Arrow((***)) )
import           Data.Attoparsec.ByteString.Char8
                                                ( peekChar
                                                , try
                                                , many1
                                                , parse
                                                , isDigit
                                                , sepBy1
                                                , parseOnly
                                                , string
                                                , takeWhile1
                                                , char
                                                , choice
                                                , Parser
                                                , IResult(Done)
                                                )
import           Debug.Trace                    ( trace )
import           AoCUtils                       ( breakOnBlankLines )

-- (Rules, Data)
type Input = ([Rule], [B.ByteString])

-- (Rule#, Definition, Dependencies)
type Rule = (Int, B.ByteString, S.IntSet)

parseRule bs = (ruleNumber, definition, dependencies)
 where
  Just (ruleNumber, definition) = second (B.drop 2) <$> B.readInt bs
  dependencies =
    S.fromList
      $ filter (/= ruleNumber)
      $ mapMaybe (fmap fst . B.readInt)
      $ B.split ' ' definition

orderByDependencies :: [Rule] -> [Rule]
orderByDependencies rules = orderByDependencies rules [] S.empty
 where
  orderByDependencies [] orderedRules _ = orderedRules
  orderByDependencies remainingRules orderedRules satisfiedDependencies =
    orderByDependencies unsatisfied (orderedRules ++ satisfied)
      $ S.union satisfiedDependencies
      $ S.fromList
      $ map (\(num, _, _) -> num) satisfied
   where
    (satisfied, unsatisfied) =
      (map fst *** map fst) $ partition (S.null . snd) $ map
        withRemainingDependencies
        remainingRules
    withRemainingDependencies rule@(_, _, dependencies) =
      (rule, dependencies S.\\ satisfiedDependencies)

parseTerminal :: Parser (Parser B.ByteString)
parseTerminal = do
  char '"'
  terminal <- takeWhile1 (/= '"')
  char '"'
  return $ string terminal

parseSequentialRules
  :: M.IntMap (Parser B.ByteString) -> Parser (Parser B.ByteString)
parseSequentialRules parsersMap = do
  rules <- sepBy1 (fst . fromJust . B.readInt <$> takeWhile1 isDigit) " "
  let parsers = mapMaybe (`M.lookup` parsersMap) rules
  if length parsers == length rules
    then return $ foldl1 (>>) parsers
    else handleRecursive parsersMap ((\x -> trace (show x) x) $ rules)

handleRecursive
  :: M.IntMap (Parser B.ByteString) -> [Int] -> Parser (Parser B.ByteString)
handleRecursive parsersMap rules = case rules of
  [42, 8] -> return $ B.concat <$> many1 (fromJust $ M.lookup 42 parsersMap)
  [42, 11, 31] -> return
    (do
      prefix <- many1 (fromJust $ M.lookup 42 parsersMap)
      suffix <- many1 (fromJust $ M.lookup 31 parsersMap)
      peek   <- peekChar
      if length prefix == length suffix
        then return $ B.concat $ (\x -> trace (show x) x) (prefix ++ suffix)
        else (\x -> trace (show (prefix, suffix, peek)) x) $ fail "Unbalanced"
    )
  rules -> trace ("oh fuck" ++ show rules) fail "oh no"

parseSubRules :: M.IntMap (Parser B.ByteString) -> Parser (Parser B.ByteString)
parseSubRules parsersMap = do
  sequentialRules <- sepBy1 (parseSequentialRules parsersMap) " | "
  return $ choice sequentialRules

parseRuleDefinition
  :: M.IntMap (Parser B.ByteString) -> Parser (Parser B.ByteString)
parseRuleDefinition parsersMap = do
  choice [parseTerminal, parseSubRules parsersMap]

createParsers :: [Rule] -> M.IntMap (Parser B.ByteString)
createParsers rules = foldl
  (\m (n, definition) ->
    M.insert n (getParser $ parseOnly (parseRuleDefinition m) definition) m
  )
  M.empty
  orderedRules
 where
  getParser (Right p    ) = p
  getParser (Left  error) = trace
    error
    (do
      return ""
    )
  orderedRules = map (\(number, definition, _) -> (number, definition))
    $ orderByDependencies rules

filterValidData :: Input -> [B.ByteString]
filterValidData (rules, inputData) = filter
  (parserCompleted . parse parser . (\x -> trace (show x) x))
  inputData
 where
  parserCompleted (Done "" _) = True
  parserCompleted _           = False
  parser = fromJust $ M.lookup 0 $ createParsers rules

loadInput :: String -> IO Input
loadInput fileName = do
  [rules, inputData] <- map B.lines . breakOnBlankLines <$> B.readFile
    ("src/" ++ fileName)
  return (map parseRule rules, inputData)
