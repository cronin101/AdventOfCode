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

immediateOrLookup :: State -> Char -> Int -> Int
immediateOrLookup state@(State _ tape _ _ _ relativeBase) m p =
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

getNextPc :: State -> Int
getNextPc state =
  programCounter state + 2 + (fst . getArityAndOperation $ state)

tapeLookup :: IntMap Int -> Int -> Int
tapeLookup tape index = fromMaybe 0 $ IntMap.lookup index tape

getArityAndOperation :: State -> (Int, [Int] -> Int)
getArityAndOperation state@(State _ tape _ inputs _ relativeBase) =
  case (getTapeHead $ state) of
    tapeHead ->
      case (getLastDigit tapeHead) of
        1 -> (2, opWithModes (+))
        2 -> (2, opWithModes (*))
        3 -> (0, \_ -> head inputs)
        4 -> (0, undefined)
        5 -> (1, undefined)
        6 -> (1, undefined)
        7 -> (2, opWithModes (testToInt (<)))
        8 -> (2, opWithModes (testToInt (==)))
        9 -> (0, undefined)
        _ -> error $ show state
      where [m1, m2, m3] = getModes tapeHead
            opWithModes op [p1, p2] =
              op (immediateOrLookup state m1 p1) (immediateOrLookup state m2 p2)

getJumpLocation :: (Int -> Bool) -> State -> Int
getJumpLocation test state@(State _ tape _ _ _ relativeBase)
  | test (immediateOrLookup state m1 p1) = immediateOrLookup state m2 p2
  | otherwise = nextPc
  where
    nextPc = getNextPc state
    [m1, m2, m3] = getModes . getTapeHead $ state
    p1 = tapeLookup tape (nextPc - 2)
    p2 = tapeLookup tape (nextPc - 1)

getResultLocation :: State -> Int
getResultLocation state =
  tapeLookup (programTape state) (programCounter state + arity + 1)
  where
    arity = fst $ getArityAndOperation state

processProgram :: State -> State
processProgram state@(State pc tape outputs inputs _ relativeBase) =
  case (tapeLookup tape pc) of
    99 -> state {terminated = True}
    op
      -- Expecting an input and we have exhausted, yield in an unfinished state
      | getLastDigit op == 3 && null inputs -> state
      | otherwise ->
        processProgram $
        case (getLastDigit op) of
          4 ->
            nextState
              {outputs = (immediateOrLookup state m1 resultLocation) : outputs}
          5 -> nextState {programCounter = getJumpLocation (/= 0) state}
          6 -> nextState {programCounter = getJumpLocation (== 0) state}
          9 ->
            nextState
              { relativeBase =
                  relativeBase + (immediateOrLookup state m1 resultLocation)
              }
          d
            | d == 3 ->
              nextState
                { programTape =
                    IntMap.insert (storeLocation m1) (head inputs) tape
                , inputs = (tail inputs)
                }
            | otherwise ->
              nextState {programTape = (setValue modes $ performOp)}
      where modes@[m1, _, _] = getModes op
  where
    nextState = state {programCounter = getNextPc state}
    (arity, operation) = getArityAndOperation state
    resultLocation = tapeLookup tape (pc + arity + 1)
    storeLocation mode =
      case mode of
        '0' -> resultLocation
        '2' -> resultLocation + relativeBase
    performOp = operation $ arguments
    arguments = map (tapeLookup tape) $ take arity $ [pc + 1 ..]
    setValue [_, _, m3] value = IntMap.insert (storeLocation m3) value tape

newProgram :: [Int] -> [Int] -> State
newProgram tape inputs =
  State 0 (IntMap.fromList (zip [0 ..] tape)) [] inputs False 0

main :: IO ()
main = do
  tape <- map (read :: String -> Int) . splitOn "," <$> readFile "./input.txt"
  putStrLn "Part One:"
  print $ outputs $ processProgram $ newProgram tape [1]
  putStrLn "Part Two:"
  print $ outputs $ processProgram $ newProgram tape [2]
