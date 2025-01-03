{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Monad (liftM2)
import Control.Monad.State qualified as ST
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (findIndex, isPrefixOf)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.PSQueue (Binding ((:->)))
import Data.PSQueue qualified as PSQ
import Data.Set qualified as S
import Data.Tuple (swap)

data NumPad = Num Int | Submit deriving (Show, Eq, Ord)

data Key = LeftKey | RightKey | UpKey | DownKey | EnterKey deriving (Show, Eq, Ord)

-- The position state of the robot's arm on the numeric keypad and the sequence of keys pressed
type PositionState = (NumPad, [Key])

-- The state of the robot's output and its position state
type State = (String, PositionState)

type Coord2D = (Int, Int)

-- Converts the input string to the corresponding NumPad button
input :: String -> NumPad
input "A" = Submit
input n = Num (read n)

-- The layout of the numeric keypad on the door
-- >>> numpadLayout
-- fromList [((0,1),Num 1),((0,2),Num 4),((0,3),Num 7),((1,0),Num 0),((1,1),Num 2),((1,2),Num 5),((1,3),Num 8),((2,0),Submit),((2,1),Num 3),((2,2),Num 6),((2,3),Num 9)]
numpadLayout :: M.Map (Int, Int) NumPad
numpadLayout =
  let keys =
        [ [Just $ Num 7, Just $ Num 8, Just $ Num 9],
          [Just $ Num 4, Just $ Num 5, Just $ Num 6],
          [Just $ Num 1, Just $ Num 2, Just $ Num 3],
          [Nothing, Just $ Num 0, Just Submit]
        ]
   in M.fromList [((x, y), k) | x <- [0 .. 2], y <- [0 .. 3], let key = keys !! (3 - y) !! x, isJust key, let Just k = key]

-- The reverse index of the numeric keypad layout for quick lookup
reverseIndexNumpad :: M.Map NumPad (Int, Int)
reverseIndexNumpad = M.fromList $ map swap $ M.toList numpadLayout

-- The layout of the directional keypad used to control the robot's arm
-- >>> arrowpadLayout
-- fromList [((0,0),LeftKey),((1,0),DownKey),((1,1),UpKey),((2,0),RightKey),((2,1),EnterKey)]
arrowpadLayout :: M.Map (Int, Int) Key
arrowpadLayout =
  let keys =
        [ [Nothing, Just UpKey, Just EnterKey],
          [Just LeftKey, Just DownKey, Just RightKey]
        ]
   in M.fromList [((x, y), k) | x <- [0 .. 2], y <- [0 .. 1], let key = keys !! (1 - y) !! x, isJust key, let Just k = key]

-- The reverse index of the directional keypad layout for quick lookup
reverseIndexArrowpad :: M.Map Key (Int, Int)
reverseIndexArrowpad = M.fromList $ map swap $ M.toList arrowpadLayout

-- Finds the fastest sequence of key presses to type the given string on the numeric keypad
-- >>> fastestDials Submit "029A"
-- [[LeftKey,EnterKey,UpKey,EnterKey,RightKey,UpKey,UpKey,EnterKey,DownKey,DownKey,DownKey,EnterKey],[LeftKey,EnterKey,UpKey,EnterKey,UpKey,RightKey,UpKey,EnterKey,DownKey,DownKey,DownKey,EnterKey],[LeftKey,EnterKey,UpKey,EnterKey,UpKey,UpKey,RightKey,EnterKey,DownKey,DownKey,DownKey,EnterKey]]
fastestDials :: NumPad -> String -> [[Key]]
fastestDials start str =
  let startState = ("", (start, []))
      goalState = (str, (input $ pure $ last str, []))
   in S.toList $ S.map reverse $ snd (dijkstra startState goalState M.! goalState)

-- Finds the fastest sequence of key presses to move from one key to another on the directional keypad
-- >>> fastestOptions EnterKey LeftKey
-- [[LeftKey,DownKey,LeftKey,EnterKey],[DownKey,LeftKey,LeftKey,EnterKey]]
fastestOptions :: Key -> Key -> [[Key]]
fastestOptions start end =
  let startState = ("", (Num 5, [start]))
      endState = case end of
        EnterKey -> ("5", (Num 5, [end]))
        LeftKey -> ("", (Num 4, [end]))
        RightKey -> ("", (Num 6, [end]))
        UpKey -> ("", (Num 8, [end]))
        DownKey -> ("", (Num 2, [end]))
   in S.toList $ S.map reverse $ snd (dijkstra startState endState M.! endState)

-- Implements Dijkstra's algorithm to find the shortest path of key presses to reach the target state
dijkstra :: State -> State -> M.Map State (Int, S.Set [Key])
dijkstra startState (targetOutput, _) = go M.empty $ PSQ.fromList [(Nothing, Nothing, startState) :-> 0]
  where
    -- Explores the edges of the graph to find the shortest path to the target state
    go :: M.Map State (Int, S.Set [Key]) -> PSQ.PSQ (Maybe State, Maybe Key, State) Int -> M.Map State (Int, S.Set [Key])
    go costs edges = case PSQ.minView edges of
      Nothing -> costs -- No more edges to explore
      Just ((previous, previousKey, edge) :-> cost, edges') -> case M.lookup edge costs of
        Just (c, histories)
          | c == cost ->
              let histories' = maybe (S.singleton []) snd ((`M.lookup` costs) =<< previous)
                  histories'' = S.map (\h -> maybe h (: h) previousKey) histories'
                  costs' = M.insert edge (cost, S.union histories histories'') costs
               in go costs' edges' -- Already visited
          | otherwise -> go costs edges' -- Already visited
        Nothing ->
          -- New edge
          let os = filter (\(_, o@(output, _)) -> o /= edge && output `isPrefixOf` targetOutput) $ options edge
              edges'' = foldl (\acc (key, e) -> PSQ.insert (Just edge, Just key, e) (cost + 1) acc) edges' os
              histories = maybe (S.singleton []) snd ((`M.lookup` costs) =<< previous)
              histories' = S.map (\h -> maybe h (: h) previousKey) histories
              costs' = M.insert edge (cost, histories') costs
           in go costs' edges''

    -- Moves the robot's arm on the directional keypad
    move :: Coord2D -> Key -> Coord2D
    move c EnterKey = c
    move (x, y) LeftKey = (x - 1, y)
    move (x, y) RightKey = (x + 1, y)
    move (x, y) UpKey = (x, y + 1)
    move (x, y) DownKey = (x, y - 1)

    -- Moves the robot's arm on the directional keypad and returns the new key it is pointing at
    moveArrowpad :: Key -> Key -> Key
    moveArrowpad k d = fromMaybe k (move (reverseIndexArrowpad M.! k) d `M.lookup` arrowpadLayout)

    -- Moves the robot's arm on the numeric keypad and returns the new key it is pointing at
    moveNumpad :: NumPad -> Key -> NumPad
    moveNumpad n d = fromMaybe n (move (reverseIndexNumpad M.! n) d `M.lookup` numpadLayout)

    -- Presses a key on the keypad and returns the new state
    press :: State -> State
    press (output, (Num n, [])) = (output ++ show n, (Num n, []))
    press (output, (Submit, [])) = (output ++ "A", (Submit, []))
    press (output, (n, keys@(arrow : ks)))
      | all (== EnterKey) keys =
          let o = case n of
                Num n' -> show n'
                Submit -> "A"
           in (output ++ o, (n, keys))
      | all (== EnterKey) ks = (output, (moveNumpad n arrow, keys))
      | otherwise =
          let revKeys = reverse keys
              Just firstNonEnter = findIndex (/= EnterKey) revKeys
              (enters, arrow : key : ks) = splitAt firstNonEnter revKeys
           in let keys' = reverse (enters ++ arrow : moveArrowpad key arrow : ks)
               in (output, (n, keys'))

    -- Returns the possible key presses and the resulting states from the current state
    options :: State -> [(Key, State)]
    options s@(output, (n, [])) =
      (EnterKey, press s)
        : [ (d, (output, (n', []))) | let p = reverseIndexNumpad M.! n, d <- [LeftKey, RightKey, UpKey, DownKey], let p' = move p d, M.member p' numpadLayout, let n' = numpadLayout M.! p'
          ]
    options s@(output, (n, keys)) =
      let k : ks = reverse keys
       in (EnterKey, press s)
            : [ (d, (output, (n, reverse (k' : ks)))) | let p = reverseIndexArrowpad M.! k, d <- [LeftKey, RightKey, UpKey, DownKey], let p' = move p d, M.member p' arrowpadLayout, let k' = arrowpadLayout M.! p'
              ]

-- Parses the input file to extract the door codes
parseCodes :: A.Parser [String]
parseCodes = map BSC.unpack <$> A.takeWhile1 (/= '\n') `A.sepBy` A.endOfLine

-- Loads the input file and parses the door codes
-- >>> loadInput "example.txt"
-- ["029A","980A","179A","456A","379A"]
loadInput :: [Char] -> IO [String]
loadInput = (fromRight [] . A.parseOnly parseCodes <$>) . BSC.readFile . ("src/" ++)

-- Extracts the numeric part of the code
numericPart :: String -> Int
numericPart = read . takeWhile (/= 'A')

-- Calculates the sum of the complexities of the door codes for part 1
-- >>> part1 <$> loadInput "example.txt"
-- 126384
part1 :: [String] -> Int
part1 = sum . map (liftM2 (*) (countStepsWithIterations 2) numericPart)

-- Calculates the sum of the complexities of the door codes for part 2
-- >>> part2 <$> loadInput "example.txt"
-- 154115708116294
part2 :: [String] -> Int
part2 = sum . map (liftM2 (*) (countStepsWithIterations 25) numericPart)

-- Counts the steps required to type the code with the given number of iterations
-- >>> map (countStepsWithIterations 25) <$> loadInput "example.txt"
-- [82050061710,72242026390,81251039228,80786362258,77985628636]
countStepsWithIterations :: Int -> [Char] -> Int
countStepsWithIterations n str = ST.evalState go M.empty
  where
    -- Finds the fastest way to type the code
    go :: ST.State (M.Map ([Key], Int) Int) Int
    go = minimum <$> mapM (countStepsForDialIteration n) (fastestDials Submit str)

    -- Counts the steps required for a single iteration of dialing the code
    countStepsForDialIteration :: Int -> [Key] -> ST.State (M.Map ([Key], Int) Int) Int
    -- Base case: no more iterations
    countStepsForDialIteration 0 ks = return $ length ks
    countStepsForDialIteration n ks = do
      -- Memoizes the results to avoid recalculating the same values
      memo <- ST.get
      -- Checks if the result is already memoized
      case (ks, n) `M.lookup` memo of
        Just c -> return c
        Nothing -> do
          -- Finds the fastest way to type the code
          let fastestOptionsForEachWithPrevious = zipWith fastestOptions (EnterKey : ks) ks
          -- Counts the steps required for the fastest way to type the code
          count <- sum . map minimum <$> mapM (mapM (countStepsForDialIteration (n - 1))) fastestOptionsForEachWithPrevious
          -- Memoizes the result
          ST.modify' (M.insert (ks, n) count)
          return count
