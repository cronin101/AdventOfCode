module Lib
  ( loadInput
  , countContainerBags
  , countContainedBags
  , toDualRules
  ) where

import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BSC
import           Data.Attoparsec.ByteString.Char8
                                                ( choice
                                                , string
                                                , sepBy1
                                                , takeTill
                                                , space
                                                , Parser
                                                , digit
                                                , parse
                                                , feed
                                                , manyTill
                                                , anyChar
                                                , endOfLine
                                                , IResult(Done)
                                                )
import           Data.Char                      ( digitToInt
                                                , isPunctuation
                                                )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.Dequeue                   ( BankersDequeue )
import qualified Data.Dequeue                  as D
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )

type BagColour = ByteString
type BagColourWithCount = (BagColour, Int)
type Rule = (BagColour, [BagColourWithCount])

-- Maps a bag to the bags that can go inside it
type CanContainMap = Map BagColour [BagColourWithCount]

-- Maps a bag to the bags that it can go inside
type CanBeContainedWithinMap = Map BagColour (Set BagColour)

type DualRules = (CanContainMap, CanBeContainedWithinMap)

-- "no other bags."
parseNoOtherBags :: Parser (Maybe BagColourWithCount)
parseNoOtherBags = do
  string $ BSC.pack "no other bags."
  return Nothing

-- "6 cherry red bags."
parseNonEmptyBagCount :: Parser (Maybe BagColourWithCount)
parseNonEmptyBagCount = do
  count <- digitToInt <$> digit
  space
  rest <- takeTill isPunctuation
  let withoutSuffix suffix = BSC.take (BSC.length rest - length suffix) rest
  let colour =
        if count == 1 then withoutSuffix " bag" else withoutSuffix " bags"
  anyChar -- punctuation
  return $ Just (colour, count)

parseBagCount :: Parser (Maybe BagColourWithCount)
parseBagCount = do
  choice [parseNoOtherBags, parseNonEmptyBagCount]

-- "6 cherry red bags, 1 golden yellow bag."
parseBagCounts :: Parser [BagColourWithCount]
parseBagCounts = do
  bagCounts <- sepBy1 parseBagCount $ string $ BSC.pack " "
  return $ catMaybes bagCounts

-- "lime green bags contain 6 cherry red bags, 1 golden yellow bag."
parseRule :: Parser Rule
parseRule = do
  bagType <- BSC.pack <$> manyTill anyChar (string $ BSC.pack " bags contain ")
  bagCounts <- parseBagCounts
  return (bagType, bagCounts)

parseRules :: Parser [Rule]
parseRules = do
  sepBy1 parseRule endOfLine

toDualRules :: [Rule] -> DualRules
toDualRules rules = (canContainMap, canBeContainedWithinMap)
 where
  canContainMap = M.fromList rules
  canBeContainedWithinMap =
    M.unionsWith S.union (map canContainToCanBeContained rules)
  canContainToCanBeContained (containerColour, innerColours) = M.fromList
    $ zip (map fst innerColours) (repeat $ S.singleton containerColour)

-- Find all bag colours that can be the container for a given bag
findContainerBags :: CanBeContainedWithinMap -> BagColour -> Set BagColour
findContainerBags canBeContainedWithinMap bagColour =
  findContainerBagsRec canBeContainedWithinMap
                       initialBag
                       (D.fromList [bagColour])
    S.\\ initialBag
  where initialBag = S.singleton bagColour

-- Recursively finds all bag colours that can be the containers for a given set of bags via BFS
findContainerBagsRec
  :: CanBeContainedWithinMap
  -> Set BagColour
  -> BankersDequeue BagColour
  -> Set BagColour
findContainerBagsRec containedMap seenColours frontier
  | D.null frontier = seenColours
  | otherwise       = findContainerBagsRec containedMap seenColours' frontier'
 where
  Just (colour, remainingFrontier) = D.popBack frontier
  containers = fromMaybe S.empty (M.lookup colour containedMap)
  seenColours' = S.union seenColours containers
  frontier' =
    foldl D.pushFront remainingFrontier $ S.toList $ containers S.\\ seenColours

countContainerBags :: CanBeContainedWithinMap -> BagColour -> Int
countContainerBags containedMap colour =
  S.size $ findContainerBags containedMap colour

countContainedBags :: CanContainMap -> BagColour -> Int
countContainedBags containsMap colour = case M.lookup colour containsMap of
  Nothing        -> 0
  Just innerBags -> sum $ map countInnerBag innerBags
 where
  countInnerBag (colour', n) = n * (1 + countContainedBags containsMap colour')

loadInput :: String -> IO [Rule]
loadInput fileName = do
  rulesRaw <- BSC.readFile ("src/" ++ fileName)
  return $ getParsedRules $ runParserOrRules rulesRaw
 where
  runParserOrRules rulesRaw = feed (parse parseRules rulesRaw) BSC.empty
  getParsedRules (Done _ rules) = rules
