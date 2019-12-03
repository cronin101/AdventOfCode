import           Data.Function
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

data Direction
  = U
  | R
  | L
  | D
  deriving (Show, Eq, Read)

data TraversalState =
  TraversalState
    { wireCount       :: Int
    -- Mapping from coordinate, to the distances for each wire at that coordinate
    , visitedSquares  :: Map (Int, Int) (Map Int Int)
    , current         :: (Int, Int)
    , currentDistance :: Int
    }
  deriving (Show)

-- Takes "R3" and spits out [R,R,R]
parseEdge :: String -> [Direction]
parseEdge edge =
  case edge of
    d:n -> replicate (read n :: Int) (read [d] :: Direction)
    _   -> error $ "Invalid edge format: " ++ edge

-- Takes "R3,U2" and spit out [R,R,R,U,U]
parseInputString :: String -> [Direction]
parseInputString = concatMap parseEdge . splitOn ","

getDirectionVector :: Direction -> (Int, Int)
getDirectionVector d =
  case d of
    U -> (0, 1)
    R -> (1, 0)
    L -> (-1, 0)
    D -> (0, -1)

traverseDirection :: Direction -> TraversalState -> TraversalState
traverseDirection d (TraversalState wireCount visited (x, y) distance) =
  TraversalState wireCount newVisted newCurrent (distance + 1)
  where
    moveCurrent diff@(dx, dy) = (x + dx, y + dy)
    newCurrent = moveCurrent $ getDirectionVector d
    distanceForWire = Map.fromList [(wireCount, distance)]
    newVisted = Map.insertWith Map.union newCurrent distanceForWire visited

traverseDirections :: [Direction] -> TraversalState -> TraversalState
traverseDirections = flip $ foldl (flip traverseDirection)

initialTraversalState :: TraversalState
initialTraversalState = TraversalState 0 Map.empty (0, 0) 1

startNewWire :: TraversalState -> TraversalState
startNewWire (TraversalState w v c l) = TraversalState (w + 1) v (0, 0) 1

traverseInputString :: String -> TraversalState -> TraversalState
traverseInputString string state =
  traverseDirections (parseInputString string) $ startNewWire state

getIntersections :: TraversalState -> [(Int, Int)]
getIntersections =
  map fst . filter ((> 1) . Map.size . snd) . Map.toList . visitedSquares

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

travelledDistance :: TraversalState -> (Int, Int) -> Maybe Int
travelledDistance state (x, y) =
  sum . map snd . Map.toList <$> Map.lookup (x, y) (visitedSquares state)

main :: IO ()
main = do
  inputStrings <- lines <$> readFile "./input.txt"
  let state = foldr traverseInputString initialTraversalState inputStrings
  putStr "Part 1: "
  print $ minimum $ map manhattanDistance $ getIntersections state
  putStr "Part 2:"
  print $ minimum $ map (travelledDistance state) $ getIntersections state
