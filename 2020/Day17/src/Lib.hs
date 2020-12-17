{-# LANGUAGE Strict #-}

module Lib
  ( loadInput
  , stepUntil
  , activeCount
  , Space(ThreeDimensions, FourDimensions)
  ) where

import qualified Data.ByteString.Char8         as B
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Control.Monad                  ( ap )
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromJust )

data Space = ThreeDimensions | FourDimensions

-- 4d Coordinate (x, y, z, w)
type C4 = (Int, Int, Int, Int)

-- WorldState: (Space, ActiveCells, LiveCells, KnownNeighbours)
type WState = (Space, S.Set C4, S.Set C4, M.Map C4 (S.Set C4))

-- (Round, WorldState) pair
type State = (Int, WState)

computeNeighbours :: Space -> C4 -> S.Set C4
computeNeighbours ThreeDimensions (x, y, z, 0) = S.fromList
  [ (x + xD, y + yD, z + zD, 0)
  | xD <- [-1 .. 1]
  , yD <- [-1 .. 1]
  , zD <- [-1 .. 1]
  , (xD, yD, zD) /= (0, 0, 0)
  ]

computeNeighbours FourDimensions (x, y, z, w) = S.fromList
  [ (x + xD, y + yD, z + zD, w + wD)
  | xD <- [-1 .. 1]
  , yD <- [-1 .. 1]
  , zD <- [-1 .. 1]
  , wD <- [-1 .. 1]
  , (xD, yD, zD, wD) /= (0, 0, 0, 0)
  ]

precalculateActiveNeighbours
  :: Space -> M.Map C4 (S.Set C4) -> S.Set C4 -> M.Map C4 (S.Set C4)
precalculateActiveNeighbours space = foldl' ensure
 where
  ensure map c =
    if M.member c map then map else M.insert c (computeNeighbours space c) map

nextActiveCells :: S.Set C4 -> S.Set C4 -> M.Map C4 (S.Set C4) -> S.Set C4
nextActiveCells liveCells activeCells knownNeighbours = S.union
  activeLiveCells
  deadActiveCells
 where
  deadActiveCells = activeCells S.\\ liveCells
  activeLiveCells = S.filter isActive liveCells
  isActive cell = becomesActive || staysActive
   where
    wasActive     = S.member cell activeCells
    becomesActive = not wasActive && activeNeighbourCount == 3
    staysActive =
      wasActive && activeNeighbourCount >= 2 && activeNeighbourCount <= 3
    neighbours           = fromJust $ M.lookup cell knownNeighbours
    activeNeighbourCount = length $ S.intersection neighbours activeCells

step :: State -> State
step (iteration, (space, activeCells, liveCells, knownNeighbours)) =
  (iteration', (space, activeCells', liveCells', knownNeighbours'))
 where
  iteration' = iteration + 1
  knownNeighbours' =
    precalculateActiveNeighbours space knownNeighbours liveCells
  activeCells' = nextActiveCells liveCells activeCells knownNeighbours'
  changedCells =
    (activeCells S.\\ activeCells') `S.union` (activeCells' S.\\ activeCells)
  liveCells' =
    S.unions $ map (fromJust . (`M.lookup` knownNeighbours')) $ S.toList
      changedCells

stepUntil :: Int -> State -> State
stepUntil targetIteration state@(iteration, _)
  | iteration >= targetIteration = state
  | otherwise                    = stepUntil targetIteration $ step state

activeCount :: State -> Int
activeCount (_, (_, activeCells, _, _)) = S.size activeCells

loadRows :: Space -> [B.ByteString] -> State
loadRows space rows = (0, (space, activeCells, liveCells, knownNeighbours))
 where
  activeCells = S.fromList
    [ (x, y, 0, 0) | (row, y) <- zip rows [0 ..], x <- B.elemIndices '#' row ]
  liveCells = S.unions $ M.elems knownNeighbours
  knownNeighbours =
    M.fromDistinctAscList $ map (ap (,) (computeNeighbours space)) $ S.toAscList
      activeCells

loadInput :: Space -> String -> IO State
loadInput space fileName =
  loadRows space . B.lines <$> B.readFile ("src/" ++ fileName)
