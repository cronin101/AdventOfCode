{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    startingStates,
    blueprintWithMaxGeodes,
  )
where

import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (nub)
import Data.Map qualified as M
import GHC.Base ((<|>))

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Show, Ord)

type Inventory = M.Map Resource Int

newtype Robot = Robot Resource deriving (Show, Eq, Ord)

type Blueprint = (Int, M.Map Robot Inventory)

data State = State
  { inventory :: Inventory,
    delta :: Inventory,
    bluePrint :: Blueprint,
    timeLeft :: Int,
    goal :: Maybe Robot,
    mostNeededOfResource :: M.Map Resource Int,
    bestCaseGeodes :: Int
  }
  deriving (Eq, Show)

addRobotToDelta :: Robot -> Inventory -> Inventory
addRobotToDelta (Robot r) delta = M.unionWith (+) delta $ M.fromList [(r, 1)]

startingDelta :: Inventory
startingDelta = addRobotToDelta (Robot Ore) M.empty

startingStates :: Int -> [Blueprint] -> [State]
startingStates timeLeft = map (\blueprint -> State M.empty startingDelta blueprint timeLeft Nothing (mostNeededOfResource blueprint) (sum [1 .. timeLeft]))
  where
    mostNeededOfResource :: Blueprint -> M.Map Resource Int
    mostNeededOfResource blueprint = M.fromList $ map resourceWithMaxCount resources
      where
        resourceWithMaxCount r = (r, maximum $ concatMap (map snd . filter ((== r) . fst) . M.toList) $ M.elems $ snd blueprint)
        resources = nub $ concatMap (map fst . M.toList) $ M.elems $ snd blueprint

-- >>> A.parseOnly parseRobot "obsidian robot"
-- Right (Robot Obsidian)
parseRobot :: A.Parser Robot
parseRobot =
  A.choice
    [ Robot Ore <$ "ore robot",
      Robot Clay <$ "clay robot",
      Robot Obsidian <$ "obsidian robot",
      Robot Geode <$ "geode robot"
    ]

parseResource :: A.Parser Resource
parseResource =
  A.choice
    [ Ore <$ "ore",
      Clay <$ "clay",
      Obsidian <$ "obsidian",
      Geode <$ "geode"
    ]

-- >>> A.parseOnly parseInventory "4 ore"
-- Right (fromList [(Ore,4)])

-- >>> A.parseOnly parseInventory "2 ore and 7 obsidian"
-- Right (fromList [(Ore,2),(Obsidian,7)])
parseInventory :: A.Parser Inventory
parseInventory = do
  resources <- A.sepBy (flip (,) <$> A.decimal <*> (" " *> parseResource)) " and "
  return $ M.fromList resources

-- >>> A.parseOnly parseRobotCost "Each obsidian robot costs 3 ore and 14 clay."
-- Right (Robot Obsidian,fromList [(Ore,3),(Clay,14)])
parseRobotCost :: A.Parser (Robot, Inventory)
parseRobotCost = do
  robot <- "Each " *> parseRobot <* " costs "
  cost <- parseInventory <* "."
  return (robot, cost)

parseBlueprint :: A.Parser Blueprint
parseBlueprint = do
  idNumber <- "Blueprint " *> A.decimal <* ":"
  robotCosts <- A.sepBy (A.skipSpace *> parseRobotCost) (A.endOfLine <|> A.skipSpace)
  return (idNumber, M.fromList robotCosts)

parseBlueprints :: A.Parser [Blueprint]
parseBlueprints = A.sepBy1 parseBlueprint (A.count 2 A.endOfLine <|> A.count 1 A.endOfLine)

loadInput :: String -> IO [Blueprint]
loadInput filename = fromRight [] . A.parseOnly parseBlueprints <$> BSC.readFile ("src/" ++ filename)

step :: State -> [State]
step state@(State _ _ _ 0 _ _ _) = [state]
step state@State {bluePrint, timeLeft, delta, inventory, mostNeededOfResource} = case goal state of
  Nothing -> concatMap (\g -> step $ state {goal = Just g}) sensibleGoals
    where
      possibleGoals = M.keys $ snd bluePrint
      sensibleGoals = filter sensibleGoal possibleGoals
      deltaForResource r = M.findWithDefault 0 r delta
      sensibleGoal (Robot r) = r == Geode || deltaForResource r < mostNeededOfResource M.! r
  Just robot
    | isPossible && timeLeft' >= 0 ->
        [ state
            { timeLeft = timeLeft',
              delta = delta',
              inventory = inventory',
              goal = Nothing,
              bestCaseGeodes = M.findWithDefault 0 Geode inventory' + sum (map (+ M.findWithDefault 0 Geode delta') [1 .. timeLeft'])
            }
        ]
    | isPossible ->
        [ state
            { timeLeft = 0,
              inventory = waitingInventory,
              goal = Nothing,
              bestCaseGeodes = M.findWithDefault 0 Geode waitingInventory
            }
        ]
    | otherwise -> []
    where
      delta' = addRobotToDelta robot delta
      waitingInventory = M.unionWith (+) inventory $ M.map (* timeLeft) delta
      timeLeft' = timeLeft - timeTaken
      inventory' = M.unionWith (-) (M.unionWith (+) inventory $ M.map (* timeTaken) delta) requirements
      isPossible = all (`M.member` delta) $ M.keys requirements
      requirements = snd bluePrint M.! robot
      timeTaken = 1 + maximum (M.elems $ M.mapWithKey timeTakenForResourceCount requirements)
      timeTakenForResourceCount r n = ceiling $ fromIntegral (max 0 $ n - M.findWithDefault 0 r inventory) / fromIntegral (delta M.! r)

filterImpossibleToCatchUp :: [State] -> [State]
filterImpossibleToCatchUp states = filter ((>= maxPassiveGeodes) . bestCaseGeodes) states
  where
    maxPassiveGeodes = maximum $ map (\s -> (M.findWithDefault 0 Geode (delta s) * timeLeft s) + M.findWithDefault 0 Geode (inventory s)) states

blueprintWithMaxGeodes :: State -> (Int, Int)
blueprintWithMaxGeodes state =
  head $ map (\s -> (fst (bluePrint s), bestCaseGeodes s)) solved
  where
    solved = head $ dropWhile (any ((/= 0) . timeLeft)) $ iterate (filterImpossibleToCatchUp . concatMap step) [state]
