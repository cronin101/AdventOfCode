module Lib
  ( loadInput
  , stepUntil
  , showState
  , activeCount
  ) where

import qualified Data.ByteString.Char8         as B
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Control.Monad                  ( ap )
import           Data.List                      ( intercalate
                                                , groupBy
                                                , sortBy
                                                , foldl'
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Function                  ( on )

-- 3d Coordinate (x, y, z)
type C3 = (Int, Int, Int)

-- WorldState: (ActiveCells, LiveCells, KnownNeighbours)
type WState = (S.Set C3, S.Set C3, M.Map C3 (S.Set C3))

-- (Round, WorldState) pair
type State = (Int, WState)

computeNeighbours :: C3 -> S.Set C3
computeNeighbours (x, y, z) = S.fromList
  [ (x + xD, y + yD, z + zD)
  | xD <- [-1 .. 1]
  , yD <- [-1 .. 1]
  , zD <- [-1 .. 1]
  , (xD, yD, zD) /= (0, 0, 0)
  ]

precalculateActiveNeighbours
  :: M.Map C3 (S.Set C3) -> S.Set C3 -> M.Map C3 (S.Set C3)
precalculateActiveNeighbours = foldl' ensure
 where
  ensure map c =
    if M.member c map then map else M.insert c (computeNeighbours c) map

nextActiveCells :: S.Set C3 -> S.Set C3 -> M.Map C3 (S.Set C3) -> S.Set C3
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
step (iteration, (activeCells, liveCells, knownNeighbours)) =
  (iteration', (activeCells', liveCells', knownNeighbours'))
 where
  iteration'       = iteration + 1
  knownNeighbours' = precalculateActiveNeighbours knownNeighbours liveCells
  activeCells'     = nextActiveCells liveCells activeCells knownNeighbours'
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
activeCount (_, (activeCells, _, _)) = S.size activeCells

loadRows :: [B.ByteString] -> State
loadRows rows = (0, (activeCells, liveCells, knownNeighbours))
 where
  activeCells = S.fromList
    [ (x, y, 0) | (row, y) <- zip rows [0 ..], x <- B.elemIndices '#' row ]
  liveCells = S.unions $ M.elems knownNeighbours
  knownNeighbours =
    M.fromDistinctAscList $ map (ap (,) computeNeighbours) $ S.toAscList
      activeCells

showState :: State -> String
showState (iteration, (activeCells, _, _)) =
  "Iteration "
    ++ show iteration
    ++ ": \n"
    ++ intercalate "\n\n" (map showLayer layeredCells)
    ++ "\n"
 where
  showLayer layer = intercalate
    "\n"
    [ concat
        [ (if S.member (x, y, head $ zs layer) activeCells then "#" else ".")
        | x <- [xMin layer .. xMax layer]
        ]
    | y <- [yMin layer .. yMax layer]
    ]
  xs layer = map (\(x, _, _) -> x) layer
  xMin layer = minimum $ xs layer
  xMax layer = maximum $ xs layer
  ys layer = map (\(_, y, _) -> y) layer
  yMin layer = minimum $ ys layer
  yMax layer = maximum $ ys layer
  zs layer = map (\(_, _, z) -> z) layer
  layeredCells = groupBy (\(_, _, z1) (_, _, z2) -> z1 == z2) sortedCells
  sortedCells =
    sortBy (compare `on` (\(x, y, z) -> (z, y, x))) $ S.toList activeCells

loadInput :: String -> IO State
loadInput fileName = loadRows . B.lines <$> B.readFile ("src/" ++ fileName)
