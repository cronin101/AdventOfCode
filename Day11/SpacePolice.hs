import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Debug.Trace
import           IntCode.Intcode

data Direction
  = Up
  | Ri
  | Do
  | Le
  deriving (Show, Read, Bounded, Eq, Ord, Enum)

data RobotState =
  RobotState
    { position  :: (Int, Int)
    , direction :: Direction
    , squares   :: Map (Int, Int) Int
    }

currentColour :: RobotState -> Int
currentColour (RobotState position _ squares) =
  fromMaybe 0 $ Map.lookup position squares

initialRobot :: RobotState
initialRobot = RobotState (0, 0) Up Map.empty

turnRight Le = Up
turnRight d  = succ d

turnLeft Up = Le
turnLeft d  = pred d

moveOne :: (Int, Int) -> Direction -> (Int, Int)
moveOne (x, y) direction =
  case direction of
    Up -> (x, y + 1)
    Ri -> (x + 1, y)
    Do -> (x, y - 1)
    Le -> (x - 1, y)

alterRobotWithOutput :: RobotState -> [Int] -> RobotState
alterRobotWithOutput robot [] = robot
alterRobotWithOutput robot [turnDirection, colourToPaint] =
  robot
    { squares = Map.insert (position robot) colourToPaint (squares robot)
    , position = moveOne (position robot) newDirection
    , direction = newDirection
    }
  where
    newDirection =
      (case turnDirection of
         0 -> turnLeft
         1 -> turnRight)
        (direction robot)
alterRobotWithOutput robot input = error $ show input

runRobotProgram :: RobotState -> State -> RobotState
runRobotProgram robot state =
  if terminated state
    then alterRobotWithOutput robot (outputs state)
    else runRobotProgram nextRobot nextState
  where
    nextRobot = alterRobotWithOutput robot (outputs nextState)
    nextState =
      processProgram $ state {inputs = [currentColour robot], outputs = []}

charAt :: (Int, Int) -> Map (Int, Int) Int -> String
charAt coordinate squares =
  case Map.lookup coordinate squares of
    Just 1 -> "##"
    _      -> "  "

main :: IO ()
main = do
  tape <-
    map (read :: String -> Int) . splitOn "," <$> readFile "./Day11/input.txt"
  putStrLn "Part One:"
  print $ Map.size $ squares $ runRobotProgram initialRobot $ newProgram tape []
  putStrLn "Part Two:"
  let outputTwo =
        squares $
        runRobotProgram (initialRobot {squares = Map.singleton (0, 0) 1}) $
        newProgram tape []
  let coordinates = map fst $ Map.toList outputTwo
  let minX = minimum $ map fst coordinates
  let maxX = maximum $ map fst coordinates
  let minY = minimum $ map snd coordinates
  let maxY = maximum $ map snd coordinates
  let xRange = [minX .. maxX]
  let yRange = reverse [minY .. maxY]
  let lines = [[charAt (x, y) outputTwo | x <- xRange] | y <- yRange]
  forM_ lines (putStrLn . foldr (++) "")
