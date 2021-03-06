module IntCode.Intcode
  ( State
  , programCounter
  , programTape
  , outputs
  , inputs
  , terminated
  , relativeBase
  , newProgram
  , processProgram
  ) where

import           Control.Lens
import           Data.Bool
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Ord
import           Debug.Trace

data State =
  State
    { programCounter :: Int
    , programTape    :: IntMap Int
    , outputs        :: [Int]
    , inputs         :: [Int]
    , terminated     :: Bool
    , relativeBase   :: Int
    }
  deriving (Show)

getModes :: Int -> [Char]
getModes tapeHead =
  case show tapeHead of
    m3:m2:m1:_:_:[] -> [m1, m2, m3]
    m2:m1:_:_:[]    -> [m1, m2, '0']
    m1:_:_:[]       -> [m1, '0', '0']
    _               -> ['0', '0', '0']

fetch :: State -> Char -> Int -> Int
fetch state@(State _ tape _ _ _ relativeBase) m p =
  case m of
    '2' -> tapeLookup tape (relativeBase + p)
    '1' -> p
    '0' -> tapeLookup tape p

testToInt :: (Int -> Int -> Bool) -> Int -> Int -> Int
testToInt op x y = bool 0 1 (op x y)

getLastDigit :: Int -> Int
getLastDigit = read . return . last . show

getTapeHead :: State -> Int
getTapeHead state = tapeLookup (programTape state) (programCounter state)

tapeLookup :: IntMap Int -> Int -> Int
tapeLookup tape index = fromMaybe 0 $ IntMap.lookup index tape

getArityOfHeadOp :: State -> Int
getArityOfHeadOp state =
  case (getLastDigit . getTapeHead $ state) of
    1 -> 2
    2 -> 2
    3 -> 0
    4 -> 0
    5 -> 1
    6 -> 1
    7 -> 2
    8 -> 2
    9 -> 0

getParameters :: State -> [Int]
getParameters state =
  take (getArityOfHeadOp state) $
  map (tapeLookup $ programTape state) $ [(programCounter state) + 1 ..]

getLocationParam :: State -> Int
getLocationParam state@(State pc tape _ _ _ _) =
  tapeLookup (tape) (pc + (getArityOfHeadOp state) + 1)

getStoreLocation :: State -> Char -> Int
getStoreLocation state mode =
  case mode of
    '0' -> locationParam
    '2' -> locationParam + (relativeBase state)
  where
    locationParam = getLocationParam state

getNextState :: State -> State
getNextState state@(State pc tape outputs inputs _ relativeBase) =
  case (getTapeHead $ state) of
    tapeHead ->
      case (getLastDigit tapeHead) of
        1 -> stateWithStore $ operation (+)
        2 -> stateWithStore $ operation (*)
        3 -> (stateWithStore (const $ head inputs)) {inputs = tail inputs}
        4 ->
          stateWithoutJump
            {outputs = (fetch state m1 (getLocationParam state)) : outputs}
        5 ->
          stateWithJump $
          getJumpLocation (/= 0) state (programCounter stateWithoutJump)
        6 ->
          stateWithJump $
          getJumpLocation (== 0) state (programCounter stateWithoutJump)
        7 -> stateWithStore $ operation (testToInt (<))
        8 -> stateWithStore $ operation (testToInt (==))
        9 ->
          stateWithShiftedRelativeBase $ fetch state m1 $ getLocationParam state
        _ -> error $ show state
      where arity = getArityOfHeadOp state
            modes@[m1, m2, m3] = getModes tapeHead
            stateWithShiftedRelativeBase shift =
              stateWithoutJump {relativeBase = relativeBase + shift}
            stateWithJump jumpLocation = state {programCounter = jumpLocation}
            stateWithoutJump = state {programCounter = (pc + arity + 2)}
            stateWithStore operation =
              stateWithoutJump
                {programTape = (setValue $ operation $ getParameters state)}
            operation op [p1, p2] = op (fetch state m1 p1) (fetch state m2 p2)
            setValue value =
              IntMap.insert (getStoreLocation state (modes !! arity)) value tape

getJumpLocation :: (Int -> Bool) -> State -> Int -> Int
getJumpLocation test state@(State programCounter tape _ _ _ relativeBase) nextPc
  | test (fetch state m1 p1) = fetch state m2 p2
  | otherwise = nextPc
  where
    [m1, m2, m3] = getModes . getTapeHead $ state
    [p1, p2] = take 2 $ map (tapeLookup tape) [programCounter + 1 ..]

processProgram :: State -> State
processProgram state@(State pc tape outputs inputs _ relativeBase) =
  case (tapeLookup tape pc) of
    99 -> state {terminated = True}
    op
        -- Expecting an input and we have exhausted, yield in an unfinished state
      | getLastDigit op == 3 && null inputs -> state
      | otherwise -> processProgram $ getNextState state

newProgram :: [Int] -> [Int] -> State
newProgram tape inputs =
  State 0 (IntMap.fromList (zip [0 ..] tape)) [] inputs False 0
