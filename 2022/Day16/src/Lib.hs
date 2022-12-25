{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    solutionA,
  )
where

import Control.Arrow ((&&&))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlpha)
import Data.Either (fromRight)
import Data.List (maximumBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import GHC.Base ((<|>))

data Valve = Valve {valveName :: String, valveFlowRate :: Int, tunnelConnections :: [String]} deriving (Show)

-- (Name to Valve, Name to Connections, Path to Destination, Valves to Open)
type Input = (M.Map String Valve, M.Map String [String], M.Map (String, String) (Int, String), S.Set String)

data State = State {timeRemaining :: Int, nextGoal :: Maybe String, pressureReleased :: Int, pressureTick :: Int, openedValves :: S.Set String, routeTaken :: [String]} deriving (Show)

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
  return (nameToValve, connections, buildLookupTable connections, S.fromList $ map fst $ filter ((> 0) . valveFlowRate . snd) $ M.toList nameToValve)

loadInput :: String -> IO Input
loadInput = (fromRight (M.empty, M.empty, M.empty, S.empty) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

startState :: State
startState = State 30 Nothing 0 0 S.empty ["AA"]

futures :: Input -> State -> [State]
futures _ (State _ _ _ _ _ []) = error "Need to have current location"
-- No future states if time is up
futures _ state@(State 0 _ _ _ _ _) = [state]
futures
  input@( nameToValve,
          _,
          routingTable,
          valvesToOpen
          )
  state@(State time goal pressure dPressure valvesOpen route@(currentLocation : _)) =
    case goal of
      Just location
        -- Goal achieved, so open valve and reset goal
        | location == currentLocation ->
            state
              : futures
                input
                state'
                  { nextGoal = Nothing,
                    openedValves = S.insert location valvesOpen,
                    pressureTick = dPressure + valveFlowRate (nameToValve M.! location)
                  }
        -- Keep moving towards goal
        | otherwise ->
            let (_, nextLocation) = fromMaybe (0, location) $ M.lookup (currentLocation, location) routingTable
             in state : futures input state' {routeTaken = nextLocation : route}
      Nothing
        -- Nothing left to do, so just wait
        | S.null (valvesToOpen S.\\ valvesOpen) -> state : futures input state'
        -- Something left to do, explore all possible valves left to open
        | otherwise -> concatMap (\newGoal -> futures input state {nextGoal = Just newGoal}) $ S.toList $ valvesToOpen S.\\ valvesOpen
    where
      state' = state {timeRemaining = time - 1, pressureReleased = pressure + pressureTick state}

solutionA :: Input -> (Int, [String])
solutionA i = maximumBy (comparing fst) $ map pressureReleasedWithRoute $ filter isEndState $ futures i startState
  where
    pressureReleasedWithRoute :: State -> (Int, [String])
    pressureReleasedWithRoute (State _ _ pressure _ _ route) = (pressure, route)
    isEndState :: State -> Bool
    isEndState = (== 0) . timeRemaining

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
