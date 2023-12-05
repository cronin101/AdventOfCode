{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Range (Bound (boundValue), Range (LowerBoundRange, SingletonRange, SpanRange), difference, intersection, rangesOverlap, (+=*))

data AlmanacType = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving (Show, Eq, Ord, Enum, Bounded)

type AlmanacTransformation = (AlmanacType, AlmanacType)

type RequirementsMap = M.Map AlmanacTransformation RequirementMap

data RequirementMapEntry = RequirementMapEntry
  { sourceRange :: Range Int,
    mappingOffset :: Int
  }
  deriving (Show)

data RequirementMap = RequirementMap
  { fromTo :: AlmanacTransformation,
    entries :: [RequirementMapEntry]
  }
  deriving (Show)

data Almanac = Almanac
  { seeds :: [Range Int],
    requirementsMap :: M.Map AlmanacTransformation RequirementMap
  }
  deriving (Show)

-- >>> A.parseOnly parseSeeds "seeds: 79 14 55 13"
-- Right [SingletonRange 79,SingletonRange 14,SingletonRange 55,SingletonRange 13]
parseSeeds :: A.Parser [Range Int]
parseSeeds = "seeds:" *> A.many1 A.space *> (map SingletonRange <$> A.decimal `A.sepBy1` A.many1 A.space)

parseAlmanacType :: A.Parser AlmanacType
parseAlmanacType =
  A.choice
    [ "seed" $> Seed,
      "soil" $> Soil,
      "fertilizer" $> Fertilizer,
      "water" $> Water,
      "light" $> Light,
      "temperature" $> Temperature,
      "humidity" $> Humidity,
      "location" $> Location
    ]

-- >>> A.parseOnly parseAlmanacTransformation "seed-to-soil"
-- Right (Seed,Soil)
parseAlmanacTransformation :: A.Parser AlmanacTransformation
parseAlmanacTransformation = (,) <$> parseAlmanacType <* "-to-" <*> parseAlmanacType

-- >>> A.parseOnly parseRequirementMapEntry "52 50 48"
-- Right (RequirementMapEntry {sourceRange = 50 +=* 98, mappingOffset = 2})
parseRequirementMapEntry :: A.Parser RequirementMapEntry
parseRequirementMapEntry = do
  (drs, (srs, rl)) <- (,) <$> A.decimal <* A.space <*> ((,) <$> A.decimal <*> (A.space *> A.decimal))
  return $ RequirementMapEntry (srs +=* (srs + rl)) (drs - srs)

-- >>> A.parseOnly parseRequirementMap "seed-to-soil map:\n51 98 2\n52 50 48"
-- Right (RequirementMap {fromTo = (Seed,Soil), entries = [RequirementMapEntry {sourceRange = 98 +=* 100, mappingOffset = -47},RequirementMapEntry {sourceRange = 50 +=* 98, mappingOffset = 2}]})
parseRequirementMap :: A.Parser RequirementMap
parseRequirementMap = do
  ft <- parseAlmanacTransformation <* A.many1 A.space <* "map:" <* A.many1 A.endOfLine
  es <- parseRequirementMapEntry `A.sepBy1` A.endOfLine
  return $ RequirementMap ft es

-- >>> A.parseOnly parseAlmanac "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48"
-- Right (Almanac {seeds = [SingletonRange 79,SingletonRange 14,SingletonRange 55,SingletonRange 13], requirementsMap = fromList [((Seed,Soil),RequirementMap {fromTo = (Seed,Soil), entries = [RequirementMapEntry {sourceRange = 98 +=* 100, mappingOffset = -48},RequirementMapEntry {sourceRange = 50 +=* 98, mappingOffset = 2}]})]})
parseAlmanac :: A.Parser Almanac
parseAlmanac = do
  s <- parseSeeds <* A.many1 A.endOfLine
  mapEntries <- parseRequirementMap `A.sepBy1` A.many1 A.endOfLine
  return $ Almanac s (M.fromList $ map (\m -> (fromTo m, m)) mapEntries)

-- >>> transformRangesOneReq (RequirementMap (Seed, Soil) [RequirementMapEntry (50 +=+ 97) 2]) [SingletonRange 79]
-- [SingletonRange 81]
transformRangesOneReq :: RequirementMap -> [Range Int] -> [Range Int]
transformRangesOneReq (RequirementMap _ es) = concatMap mapRangeToRanges
  where
    mapRangeToRanges r = mapMaybe (mapRangeToRangeEntry r) es ++ [r] `difference` map sourceRange es
    mapRangeToRangeEntry r (RequirementMapEntry srs o) = if r `rangesOverlap` srs then Just (fmap (+ o) $ head $ [r] `intersection` [srs]) else Nothing

transformRangesAllReq :: RequirementsMap -> Range Int -> [Range Int]
transformRangesAllReq rm seedRange = foldl step [seedRange] allRequirements
  where
    allRequirements = zip [Seed ..] [Soil ..]
    step rs transformation = transformRangesOneReq (rm M.! transformation) rs

-- >>> loadInput "example.txt"
-- Almanac {seeds = [SingletonRange 79,SingletonRange 14,SingletonRange 55,SingletonRange 13], requirementsMap = fromList [((Seed,Soil),RequirementMap {fromTo = (Seed,Soil), entries = [RequirementMapEntry {sourceRange = 98 +=* 100, mappingOffset = -48},RequirementMapEntry {sourceRange = 50 +=* 98, mappingOffset = 2}]}),((Soil,Fertilizer),RequirementMap {fromTo = (Soil,Fertilizer), entries = [RequirementMapEntry {sourceRange = 15 +=* 52, mappingOffset = -15},RequirementMapEntry {sourceRange = 52 +=* 54, mappingOffset = -15},RequirementMapEntry {sourceRange = 0 +=* 15, mappingOffset = 39}]}),((Fertilizer,Water),RequirementMap {fromTo = (Fertilizer,Water), entries = [RequirementMapEntry {sourceRange = 53 +=* 61, mappingOffset = -4},RequirementMapEntry {sourceRange = 11 +=* 53, mappingOffset = -11},RequirementMapEntry {sourceRange = 0 +=* 7, mappingOffset = 42},RequirementMapEntry {sourceRange = 7 +=* 11, mappingOffset = 50}]}),((Water,Light),RequirementMap {fromTo = (Water,Light), entries = [RequirementMapEntry {sourceRange = 18 +=* 25, mappingOffset = 70},RequirementMapEntry {sourceRange = 25 +=* 95, mappingOffset = -7}]}),((Light,Temperature),RequirementMap {fromTo = (Light,Temperature), entries = [RequirementMapEntry {sourceRange = 77 +=* 100, mappingOffset = -32},RequirementMapEntry {sourceRange = 45 +=* 64, mappingOffset = 36},RequirementMapEntry {sourceRange = 64 +=* 77, mappingOffset = 4}]}),((Temperature,Humidity),RequirementMap {fromTo = (Temperature,Humidity), entries = [RequirementMapEntry {sourceRange = 69 +=* 70, mappingOffset = -69},RequirementMapEntry {sourceRange = 0 +=* 69, mappingOffset = 1}]}),((Humidity,Location),RequirementMap {fromTo = (Humidity,Location), entries = [RequirementMapEntry {sourceRange = 56 +=* 93, mappingOffset = 4},RequirementMapEntry {sourceRange = 93 +=* 97, mappingOffset = -37}]})]}
loadInput :: [Char] -> IO Almanac
loadInput = (fromRight (Almanac [] M.empty) . A.parseOnly parseAlmanac <$>) . BSC.readFile . ("src/" ++)

loBound :: Range a -> a
loBound (SpanRange l _) = boundValue l
loBound (SingletonRange v) = v
loBound (LowerBoundRange l) = boundValue l
loBound _ = error "Infinity"

-- >>> part1 <$> loadInput "example.txt"
-- 35
part1 :: Almanac -> Int
part1 (Almanac s rm) = minimum $ map (minimum . map loBound . transformRangesAllReq rm) s

-- >>> part2 <$> loadInput "example.txt"
-- 46
part2 :: Almanac -> Int
part2 a@(Almanac s _) =
  let a' = a {seeds = map (\[x, y] -> loBound x +=* (loBound x + loBound y)) $ chunksOf 2 s}
   in part1 a'
