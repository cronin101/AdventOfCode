{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    solutionA,
    solutionB,
    allPairedStarts,
    allReasonablePairedStarts,
  )
where

import Control.Arrow (Arrow (first, second), (&&&))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlpha)
import Data.Either (fromRight)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Base ((<|>))

data Valve = Valve {valveName :: String, valveFlowRate :: Int, tunnelConnections :: [String]} deriving (Show)

-- (Name to Valve, Routing Table, Valves to Open)
type Input = (M.Map String Valve, M.Map (String, String) (Int, String), [String])

data State = State {timeRemaining :: Int, nextGoal :: Maybe String, pressureReleased :: Int, pressureTick :: Int, currentLocation :: String} deriving (Show)

-- >>> A.parseOnly parseValve "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
-- Right (Valve {valveName = "AA", valveFlowRate = 0, tunnelConnections = ["DD","II","BB"]})
parseValve :: A.Parser Valve
parseValve = do
  (name, flowrate) <- (,) <$> ("Valve " *> A.takeWhile1 isAlpha <* " has flow rate=") <*> (A.decimal <* ("; tunnels lead to valves " <|> "; tunnel leads to valve "))
  connections <- A.sepBy (A.takeWhile1 isAlpha) ", "
  return $ Valve (BSC.unpack name) flowrate (BSC.unpack <$> connections)

parseInput :: A.Parser Input
parseInput = do
  valves <- A.sepBy1 parseValve A.endOfLine
  let nameToValve = M.fromList $ (valveName &&& id) <$> valves
  let connections = M.fromList $ (valveName &&& (mapMaybe (fmap valveName <$> (`M.lookup` nameToValve)) . tunnelConnections)) <$> valves
  let valvesWithValue = map fst $ filter ((> 0) . valveFlowRate . snd) $ M.toList nameToValve
  return (nameToValve, buildLookupTable connections, valvesWithValue)

loadInput :: String -> IO Input
loadInput = (fromRight (M.empty, M.empty, []) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

startState :: State
startState = State 30 Nothing 0 0 "AA"

futures :: Input -> State -> [State]
futures
  input@( nameToValve,
          routingTable,
          valvesToOpen
          )
  state@(State time goal pressure dPressure currentLoc)
    -- No future states if time is up, yield where we are
    | time == 0 = [state]
    | time < 0 = []
    | otherwise = case goal of
        Just location
          -- Goal achieved, so open valve and reset goal
          | location == currentLoc ->
              futures
                (nameToValve, routingTable, filter (/= location) valvesToOpen)
                (stepState 1)
                  { nextGoal = Nothing,
                    pressureTick = dPressure + valveFlowRate (nameToValve M.! location)
                  }
          -- Move completely towards goal
          | otherwise ->
              let (turnsTaken, _) = fromMaybe (1, location) $ M.lookup (currentLoc, location) routingTable
               in futures input (stepState turnsTaken) {currentLocation = location}
        Nothing
          -- Nothing left to do, so just wait
          | null valvesToOpen -> futures input (stepState time)
          -- Something left to do, explore all possible valves left to open
          | otherwise -> concatMap (\newGoal -> futures input state {nextGoal = Just newGoal}) valvesToOpen
    where
      stepState n = state {timeRemaining = time - n, pressureReleased = pressure + (n * pressureTick state)}

solutionA :: Input -> Int
solutionA i = maximum $ map pressureReleased $ futures i startState

solutionB :: Input -> Int
solutionB i = maximum $ map (\(me, elephant) -> bestOutcome (futures me startState {timeRemaining = 26}, futures elephant startState {timeRemaining = 26})) $ allReasonablePairedStarts i
  where
    bestOutcome (me, elephant) = bestIndividualOutcome me + bestIndividualOutcome elephant
    bestIndividualOutcome = maximum . map pressureReleased . filter ((== 0) . timeRemaining)

buildLookupTable :: M.Map String [String] -> M.Map (String, String) (Int, String)
buildLookupTable connections = buildLookup' M.empty M.empty
  where
    buildLookup' previous current
      | M.null current = buildLookup' current $ M.fromList $ concatMap makeInitialRoutes $ M.toList connections
      | current == previous = current
      | otherwise = buildLookup' current $ M.unionWith chooseCheapest current next
      where
        next = M.fromList $ map tieTogether $ filter adjacent crossProduct
        crossProduct = (,) <$> M.toList current <*> M.toList current
        adjacent (((start, mid), _), ((mid', end), _)) = mid == mid' && start /= end
        tieTogether
          (((start, _), (leftCost, nextHop)), ((_, end), (rightCost, _))) =
            ((start, end), (leftCost + rightCost, nextHop))
        chooseCheapest existing@(existingCost, _) new@(newCost, _) = if existingCost <= newCost then existing else new
        makeInitialRoutes (start, ends) = map (\end -> ((start, end), (1, end))) ends

-- >>> allPartitions [1, 2]
-- [([1,2],[]),([2],[1])]
allPartitions :: Ord a => [a] -> [([a], [a])]
allPartitions [] = [([], [])]
allPartitions (x : xs) = nub $ map sortTuple (concatMap (\l -> [first (x :) l, second (x :) l]) (allPartitions xs))
  where
    sortTuple (a, b) = if a > b then (a, b) else (b, a)

allReasonablePairedStarts :: Input -> [(Input, Input)]
allReasonablePairedStarts = filter (\((_, _, me), (_, _, elephant)) -> abs (length me - length elephant) <= 3) . allPairedStarts

allPairedStarts :: Input -> [(Input, Input)]
allPairedStarts
  ( nameToValve,
    routingTable,
    valvesToOpen
    ) = map (\(me, elephant) -> ((nameToValve, routingTable, me), (nameToValve, routingTable, elephant))) allValvePartitions
    where
      allValvePartitions = allPartitions valvesToOpen
