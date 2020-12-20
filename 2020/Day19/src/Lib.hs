{-# LANGUAGE Strict, OverloadedStrings #-}

module Lib
  ( loadInput
  , filterValidData
  , balancedIngestor
  , repeatingIngestor
  ) where

import qualified Data.ByteString.Char8         as B
import qualified Data.IntSet                   as S
import qualified Data.IntMap                   as M
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.List                      ( partition
                                                , foldl'
                                                )
import           Control.Arrow                  ( Arrow((***)) )
import           Data.Attoparsec.ByteString.Char8
                                                ( anyChar
                                                , try
                                                , isDigit
                                                , sepBy1
                                                , parseOnly
                                                , takeWhile1
                                                , char
                                                , choice
                                                , Parser
                                                )
import           AoCUtils                       ( breakOnBlankLines )

-- (Rules, Data)
type Input = ([Rule], [B.ByteString])

-- (Rule#, Definition, Dependencies)
type Rule = (Int, B.ByteString, S.IntSet)

type Ingestor = B.ByteString -> [B.ByteString]

parseRule :: B.ByteString -> (Int, B.ByteString, S.IntSet)
parseRule bs = (ruleNumber, definition, dependencies)
 where
  Just (ruleNumber, definition) = second (B.drop 2) <$> B.readInt bs
  dependencies =
    S.fromList $ mapMaybe (fmap fst . B.readInt) $ B.split ' ' definition

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

parseTerminal :: Parser Ingestor
parseTerminal = try $ do
  terminal <- char '"' *> anyChar <* char '"'
  return
    (\bs -> case B.uncons bs of
      Nothing        -> []
      Just (c, rest) -> [ rest | c == terminal ]
    )

ingestMany :: [Ingestor] -> Ingestor
ingestMany ingestors bs = foldl' (flip concatMap) [bs] ingestors

ingestEither :: (Ingestor, Ingestor) -> Ingestor
ingestEither (left, right) bs = left bs ++ right bs

parseSequentialRules :: M.IntMap Ingestor -> Parser Ingestor
parseSequentialRules ingestorMap = do
  rules <- sepBy1 (fst . fromJust . B.readInt <$> takeWhile1 isDigit) " "
  return $ ingestMany $ mapMaybe (`M.lookup` ingestorMap) rules

parseUnion :: M.IntMap Ingestor -> Parser Ingestor
parseUnion parsersMap = do
  sequentialRules <- sepBy1 (parseSequentialRules parsersMap) " | "
  return $ case sequentialRules of
    [single]      -> single
    [left, right] -> ingestEither (left, right)

parseRuleDefinition :: M.IntMap Ingestor -> Parser Ingestor
parseRuleDefinition ingestorMap =
  choice [parseTerminal, parseUnion ingestorMap]

createIngestors
  :: [Rule] -> M.IntMap (M.IntMap Ingestor -> Ingestor) -> M.IntMap Ingestor
createIngestors rules overrides = foldl
  (\m (n, definition, _) -> M.insert
    n
    (if M.member n overrides
      then (overrides M.! n) m
      else getParsed $ parseOnly (parseRuleDefinition m) definition
    )
    m
  )
  M.empty
  orderedRules
 where
  getParsed (Right p) = p
  orderedRules = orderByDependencies rules

filterValidData
  :: Input -> M.IntMap (M.IntMap Ingestor -> Ingestor) -> [B.ByteString]
filterValidData (rules, inputData) overrides = filter
  (any B.null . rootIngestor)
  inputData
  where rootIngestor = createIngestors rules overrides M.! 0

ingestorsFromIndices :: [Int] -> M.IntMap Ingestor -> Ingestor
ingestorsFromIndices is map = ingestMany $ mapMaybe (`M.lookup` map) is

balancedIngestor :: (Int, Int) -> M.IntMap Ingestor -> Ingestor
balancedIngestor ends@(prefix, suffix) m = ingestEither
  ( ingestorsFromIndices [prefix, suffix] m
  , ingestMany
    [ ingestorsFromIndices [prefix] m
    , balancedIngestor ends m
    , ingestorsFromIndices [suffix] m
    ]
  )

repeatingIngestor :: Int -> M.IntMap Ingestor -> Ingestor
repeatingIngestor repeater m = ingestEither
  ( ingestorsFromIndices [repeater] m
  , ingestMany [ingestorsFromIndices [repeater] m, repeatingIngestor repeater m]
  )

loadInput :: String -> IO Input
loadInput fileName = do
  [rules, inputData] <- map B.lines . breakOnBlankLines <$> B.readFile
    ("src/" ++ fileName)
  return (map parseRule rules, inputData)
