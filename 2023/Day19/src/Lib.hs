{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib (loadInput, part1, part2) where

import Control.Arrow (second)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Attoparsec.Combinator qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Range qualified as R

data Part = Part {_x :: Int, _m :: Int, _a :: Int, _s :: Int} deriving (Show, Eq, Ord)

data VirtualPart = VirtualPart {_x' :: Maybe (R.Range Int), _m' :: Maybe (R.Range Int), _a' :: Maybe (R.Range Int), _s' :: Maybe (R.Range Int)} deriving (Show, Eq)

instance Ord VirtualPart where
  compare :: VirtualPart -> VirtualPart -> Ordering
  compare (VirtualPart x m a s) (VirtualPart x' m' a' s') = compare (show (x, m, a, s)) (show (x', m', a', s'))

data Rule = Rule {constraint :: Maybe Constraint, destination :: String} deriving (Show, Eq, Ord)

data Constraint = Constraint {cText :: String, xC :: R.Range Int, mC :: R.Range Int, aC :: R.Range Int, sC :: R.Range Int} deriving (Show, Eq)

instance Ord Constraint where
  compare :: Constraint -> Constraint -> Ordering
  compare c c' = cText c `compare` cText c'

data Pipeline = Pipeline {name :: String, rules :: [Rule]} deriving (Show, Eq, Ord)

type Pipelines = M.Map String Pipeline

type Parts = M.Map String [Part]

type VirtualParts = M.Map String [VirtualPart]

type Input = (Pipelines, Parts)

type VirtualInput = (Pipelines, VirtualParts)

-- >>> A.parseOnly parseConstraint "a>1"
-- Right (Constraint {cText = "a>1", xC = inf, mC = inf, aC = lbe 1, sC = inf})
parseConstraint :: A.Parser Constraint
parseConstraint = do
  text <- A.lookAhead $ A.takeWhile (\c -> c /= ',' && c /= '}')
  let c = Constraint (BSC.unpack text) R.inf R.inf R.inf R.inf
  fieldConstraint <-
    A.choice
      [ "x" $> \r -> c {xC = r},
        "m" $> \r -> c {mC = r},
        "a" $> \r -> c {aC = r},
        "s" $> \r -> c {sC = r}
      ]
  range <- A.choice ["<" $> R.ube, ">" $> R.lbe]
  fieldConstraint . range <$> A.decimal

-- >>> A.parseOnly parseRule "a<2006:qkq"
-- Right (Rule {constraint = Just (Constraint {cText = "a<2006:qkq", xC = inf, mC = inf, aC = ube 2006, sC = inf}), destination = "qkq"})

-- >>> A.parseOnly parseRule "A"
-- Right (Rule {constraint = Nothing, destination = "A"})
parseRule :: A.Parser Rule
parseRule = do
  constraint <- A.option Nothing (Just <$> (parseConstraint <* ":"))
  destination <- A.takeWhile1 A.isAlpha_ascii
  return $ Rule constraint (BSC.unpack destination)

-- >>> A.parseOnly parsePipeline "px{a<2006:qkq,m>2090:A,rfg}"
-- Right (Pipeline {name = "px", rules = [Rule {predicate = Just a<2006:qkq, constraint = Just (Constraint {cText = "a<2006:qkq", xC = inf, mC = inf, aC = lbe 2006, sC = inf}), destination = "qkq"},Rule {predicate = Just m>2090:A, constraint = Just (Constraint {cText = "m>2090:A", xC = inf, mC = ube 2090, aC = inf, sC = inf}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "rfg"}]})
parsePipeline :: A.Parser Pipeline
parsePipeline = do
  name <- A.takeWhile1 A.isAlpha_ascii
  rules <- "{" *> (parseRule `A.sepBy1` ",") <* "}"
  return $ Pipeline (BSC.unpack name) rules

-- >>> A.parseOnly parsePart "{x=787,m=2655,a=1222,s=2876}"
-- Right (Part {_x = 787, _m = 2655, _a = 1222, _s = 2876})
parsePart :: A.Parser Part
parsePart = do
  x <- "{" *> "x=" *> A.decimal <* ","
  m <- "m=" *> A.decimal <* ","
  a <- "a=" *> A.decimal <* ","
  s <- "s=" *> A.decimal <* "}"
  return $ Part x m a s

parseInput :: A.Parser Input
parseInput = do
  pipelines <- M.fromList . map (\p -> (name p, p)) <$> (parsePipeline `A.sepBy1` A.endOfLine) <* A.count 2 A.endOfLine
  parts <- M.fromListWith (++) . map (("in",) . pure) <$> parsePart `A.sepBy1` A.endOfLine
  let terminals = M.union (M.singleton "R" $ Pipeline "R" [Rule Nothing "R"]) (M.singleton "A" $ Pipeline "A" [Rule Nothing "A"])
  return (M.union pipelines terminals, parts)

-- >>> loadInput "example.txt"
-- (fromList [("A",Pipeline {name = "A", rules = [Rule {predicate = Nothing, constraint = Nothing, destination = "A"}]}),("R",Pipeline {name = "R", rules = [Rule {predicate = Nothing, constraint = Nothing, destination = "R"}]}),("crn",Pipeline {name = "crn", rules = [Rule {predicate = Just x>2662:A, constraint = Just (Constraint {cText = "x>2662:A", xC = ube 2662, mC = inf, aC = inf, sC = inf}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "R"}]}),("gd",Pipeline {name = "gd", rules = [Rule {predicate = Just a>3333:R, constraint = Just (Constraint {cText = "a>3333:R", xC = inf, mC = inf, aC = ube 3333, sC = inf}), destination = "R"},Rule {predicate = Nothing, constraint = Nothing, destination = "R"}]}),("hdj",Pipeline {name = "hdj", rules = [Rule {predicate = Just m>838:A, constraint = Just (Constraint {cText = "m>838:A", xC = inf, mC = ube 838, aC = inf, sC = inf}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "pv"}]}),("in",Pipeline {name = "in", rules = [Rule {predicate = Just s<1351:px, constraint = Just (Constraint {cText = "s<1351:px", xC = inf, mC = inf, aC = inf, sC = lbe 1351}), destination = "px"},Rule {predicate = Nothing, constraint = Nothing, destination = "qqz"}]}),("lnx",Pipeline {name = "lnx", rules = [Rule {predicate = Just m>1548:A, constraint = Just (Constraint {cText = "m>1548:A", xC = inf, mC = ube 1548, aC = inf, sC = inf}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "A"}]}),("pv",Pipeline {name = "pv", rules = [Rule {predicate = Just a>1716:R, constraint = Just (Constraint {cText = "a>1716:R", xC = inf, mC = inf, aC = ube 1716, sC = inf}), destination = "R"},Rule {predicate = Nothing, constraint = Nothing, destination = "A"}]}),("px",Pipeline {name = "px", rules = [Rule {predicate = Just a<2006:qkq, constraint = Just (Constraint {cText = "a<2006:qkq", xC = inf, mC = inf, aC = lbe 2006, sC = inf}), destination = "qkq"},Rule {predicate = Just m>2090:A, constraint = Just (Constraint {cText = "m>2090:A", xC = inf, mC = ube 2090, aC = inf, sC = inf}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "rfg"}]}),("qkq",Pipeline {name = "qkq", rules = [Rule {predicate = Just x<1416:A, constraint = Just (Constraint {cText = "x<1416:A", xC = lbe 1416, mC = inf, aC = inf, sC = inf}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "crn"}]}),("qqz",Pipeline {name = "qqz", rules = [Rule {predicate = Just s>2770:qs, constraint = Just (Constraint {cText = "s>2770:qs", xC = inf, mC = inf, aC = inf, sC = ube 2770}), destination = "qs"},Rule {predicate = Just m<1801:hdj, constraint = Just (Constraint {cText = "m<1801:hdj", xC = inf, mC = lbe 1801, aC = inf, sC = inf}), destination = "hdj"},Rule {predicate = Nothing, constraint = Nothing, destination = "R"}]}),("qs",Pipeline {name = "qs", rules = [Rule {predicate = Just s>3448:A, constraint = Just (Constraint {cText = "s>3448:A", xC = inf, mC = inf, aC = inf, sC = ube 3448}), destination = "A"},Rule {predicate = Nothing, constraint = Nothing, destination = "lnx"}]}),("rfg",Pipeline {name = "rfg", rules = [Rule {predicate = Just s<537:gd, constraint = Just (Constraint {cText = "s<537:gd", xC = inf, mC = inf, aC = inf, sC = lbe 537}), destination = "gd"},Rule {predicate = Just x>2440:R, constraint = Just (Constraint {cText = "x>2440:R", xC = ube 2440, mC = inf, aC = inf, sC = inf}), destination = "R"},Rule {predicate = Nothing, constraint = Nothing, destination = "A"}]})],fromList [("in",[Part {_x = 2127, _m = 1623, _a = 2188, _s = 1013},Part {_x = 2461, _m = 1339, _a = 466, _s = 291},Part {_x = 2036, _m = 264, _a = 79, _s = 2244},Part {_x = 1679, _m = 44, _a = 2067, _s = 496},Part {_x = 787, _m = 2655, _a = 1222, _s = 2876}])])
loadInput :: [Char] -> IO Input
loadInput = (fromRight (M.empty, M.empty) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

testConstraint :: Part -> Constraint -> Bool
testConstraint (Part x m a s) (Constraint _ xC mC aC sC) = and $ zipWith (flip R.inRange) [x, m, a, s] [xC, mC, aC, sC]

-- >>> splitVirtualPart (VirtualPart (Just(0 R.*=* 10)) (Just(0 R.*=* 10)) (Just(0 R.*=* 10)) (Just(0 R.*=* 10))) (Constraint "x>1" (R.lbe 1) R.inf R.inf R.inf)
-- (Just (VirtualPart {_x' = Just 1 *=* 10, _m' = Just 0 *=* 10, _a' = Just 0 *=* 10, _s' = Just 0 *=* 10}),Just (VirtualPart {_x' = Just 0 *=+ 1, _m' = Just 0 *=* 10, _a' = Just 0 *=* 10, _s' = Just 0 *=* 10}))
splitVirtualPart :: VirtualPart -> Constraint -> (Maybe VirtualPart, Maybe VirtualPart)
splitVirtualPart (VirtualPart x m a s) (Constraint _ xC mC aC sC) = (validate pass, validate fail)
  where
    validate p@(VirtualPart x m a s) = if any isJust [x, m, a, s] then Just p else Nothing
    overlap a b = head $ map Just (R.intersection [a] [b]) ++ [Nothing]
    diff a b = head $ map Just (R.difference [a] [b]) ++ [Nothing]
    pass = VirtualPart ((`overlap` xC) =<< x) ((`overlap` mC) =<< m) ((`overlap` aC) =<< a) ((`overlap` sC) =<< s)
    fail = VirtualPart (Just $ fromMaybe (fromJust x) x') (Just $ fromMaybe (fromJust m) m') (Just $ fromMaybe (fromJust a) a') (Just $ fromMaybe (fromJust s) s')
      where
        x' = (`diff` xC) =<< x
        m' = (`diff` mC) =<< m
        a' = (`diff` aC) =<< a
        s' = (`diff` sC) =<< s

applyPipeline :: Pipeline -> Part -> String
applyPipeline (Pipeline _ rs) part =
  let Just match = find (maybe True (testConstraint part) . constraint) rs
   in destination match

applyVirtualPipeline :: [Rule] -> Maybe VirtualPart -> [(String, VirtualPart)]
applyVirtualPipeline [] _ = []
applyVirtualPipeline _ Nothing = []
applyVirtualPipeline (Rule Nothing dest : _) part = [(dest, fromJust part)]
applyVirtualPipeline (Rule (Just c) dest : rs) (Just part) =
  let (passed, failed) = splitVirtualPart part c
   in if isJust passed
        then (dest, fromJust passed) : applyVirtualPipeline rs failed
        else applyVirtualPipeline rs (Just part)

step :: Input -> Input
step (pipelines, parts) =
  let parts' =
        M.fromListWith (++) $
          concatMap (\(lineName, parts) -> map (\part -> (applyPipeline (pipelines M.! lineName) part, pure part)) $ reverse parts) $
            M.toList parts
   in (pipelines, parts')

virtualStep :: VirtualInput -> VirtualInput
virtualStep (pipelines, parts) =
  let parts' =
        M.fromListWith (++) $
          concatMap (\(lineName, parts) -> concatMap (map (second pure) . applyVirtualPipeline (rules $ pipelines M.! lineName) . Just) $ reverse parts) $
            M.toList parts
   in (pipelines, parts')

solveWith :: Eq a => (a -> a) -> a -> a
solveWith stepFn i = stepFn $ fst $ last $ takeWhile (uncurry (/=)) $ zip steps (drop 1 steps)
  where
    steps = iterate stepFn i

solve :: Input -> Input
solve = solveWith step

solveVirtual :: VirtualInput -> VirtualInput
solveVirtual = solveWith virtualStep

partRating :: Part -> Int
partRating (Part x m a s) = x + m + a + s

-- >>> part1 <$> loadInput "example.txt"
-- 19114
part1 :: Input -> Int
part1 = sum . map partRating . (M.! "A") . snd . solve

virtualise :: Input -> VirtualInput
virtualise (pipelines, _) = let r = Just $ 1 R.+=+ 4000 in (pipelines, M.singleton "in" [VirtualPart r r r r])

possibilities :: VirtualParts -> Int
possibilities parts = sum $ map possibilities' $ parts M.! "A"
  where
    possibilities' (VirtualPart x m a s) = product $ map (maybe 0 rangeSize) [x, m, a, s]
    rangeSize (R.SpanRange (R.Bound lo loT) (R.Bound hi hiT)) = (hi - lo) + 1 - length (filter (== R.Exclusive) [loT, hiT])

-- >>> part2 <$> loadInput "example.txt"
-- 167409079868000
part2 :: Input -> Int
part2 = possibilities . snd . solveVirtual . virtualise
