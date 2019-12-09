import           Control.Lens
import           Data.Bool
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Ord

data State =
  State
    { programCounter :: Int
    , programTape    :: [Int]
    , outputs        :: [Int]
    , inputs         :: [Int]
    , terminated     :: Bool
    , relativeBase   :: Int
    }
  deriving (Show)

getModes :: Int -> (Char, Char, Char)
getModes tapeHead =
  case show tapeHead of
    m3:m2:m1:_:_:[] -> (m1, m2, m3)
    m2:m1:_:_:[]    -> (m1, m2, '0')
    m1:_:_:[]       -> (m1, '0', '0')
    _               -> ('0', '0', '0')

immediateOrLookup :: [Int] -> Int -> Char -> Int -> Int
immediateOrLookup tape rB m p =
  case m of
    '2' -> tape !! (rB + p)
    '1' -> p
    '0' -> tape !! p

testToInt :: (Int -> Int -> Bool) -> Int -> Int -> Int
testToInt op x y = bool 0 1 (op x y)

getLastDigit :: Int -> Int
getLastDigit = read . return . last . show

getArityAndOperation ::
     Int -> Int -> Int -> (Int, (Char, Char, Char) -> [Int] -> [Int] -> Int)
getArityAndOperation tapeHead input relativeBase =
  case (getLastDigit tapeHead) of
    1 -> (2, opWithModes (+))
    2 -> (2, opWithModes (*))
    3 -> (0, \_ _ _ -> input)
    4 -> (0, undefined)
    5 -> (1, undefined)
    6 -> (1, undefined)
    7 -> (2, opWithModes (testToInt (<)))
    8 -> (2, opWithModes (testToInt (==)))
    9 -> (0, undefined)
    _ -> error $ show tapeHead
  where
    opWithModes op (m1, m2, m3) tape [p1, p2] =
      op
        (immediateOrLookup tape relativeBase m1 p1)
        (immediateOrLookup tape relativeBase m2 p2)

getJumpLocation ::
     (Int -> Bool) -> (Char, Char, Char) -> Int -> [Int] -> Int -> Int
getJumpLocation test (m1, m2, m3) relativeBase tape nextPc
  | test (immediateOrLookup tape relativeBase m1 p1) =
    immediateOrLookup tape relativeBase m2 p2
  | otherwise = nextPc
  where
    p1 = tape !! (nextPc - 2)
    p2 = tape !! (nextPc - 1)

processProgram :: State -> State
processProgram state@(State pc tape outputs inputs terminated relativeBase) =
  case (tape !! pc) of
    99 -> State pc tape outputs inputs True relativeBase
    op ->
      case (getLastDigit op) of
        4 -> processNonJumpingState tape (pushOutput (modes)) inputs
        5 -> jumpTo $ jumpLocationAfterTest (/= 0)
        6 -> jumpTo $ jumpLocationAfterTest (== 0)
        9 ->
          processProgram $
          State
            nextPc
            tape
            outputs
            inputs
            False
            (relativeBase +
             (immediateOrLookup tape relativeBase (fst3 modes) resultLocation))
        d
          -- Expecting an input and we have exhausted, yield in an unfinished state
          | d == 3 && null inputs ->
            State pc tape outputs inputs False relativeBase
          | d == 3 ->
            processNonJumpingState
              (element (storeLocation (fst3 modes)) .~ (head inputs) $ tape)
              outputs
              (tail inputs)
          | otherwise ->
            processNonJumpingState
              (setValue modes $ performOp $ modes)
              outputs
              inputs
      where modes = getModes op
            jumpLocationAfterTest test =
              getJumpLocation test modes relativeBase tape nextPc
  where
    fst3 (a, b, c) = a
    pushOutput (m1, _, _) =
      (immediateOrLookup tape relativeBase m1 resultLocation) : outputs
    jumpTo destination =
      processProgram $ State destination tape outputs inputs False relativeBase
    processNonJumpingState t o nextInputs =
      processProgram $ State nextPc t o nextInputs False relativeBase
    (arity, operation) =
      getArityAndOperation (tape !! pc) (head inputs) relativeBase
    nextPc = pc + arity + 2
    resultLocation = tape !! (nextPc - 1)
    storeLocation mode =
      case mode of
        '0' -> resultLocation
        '2' -> resultLocation + relativeBase
    performOp (m1, m2, m3) =
      operation (m1, m2, m3) tape $ take arity $ drop (pc + 1) tape
    setValue (_, _, m3) value = element (storeLocation m3) .~ value $ tape

newProgram :: [Int] -> [Int] -> State
newProgram tape inputs = State 0 (tape ++ repeat 0) [] inputs False 0

main :: IO ()
main = do
  tape <- map (read :: String -> Int) . splitOn "," <$> readFile "./input.txt"
  putStrLn "Part One:"
  print $ outputs $ processProgram $ newProgram tape [1]
  putStrLn "Part Two:"
  print $ outputs $ processProgram $ newProgram tape [2]
