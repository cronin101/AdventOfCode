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
    }
  deriving (Show)

getModes :: Int -> (Char, Char)
getModes tapeHead =
  case show tapeHead of
    m2:m1:_:_:[] -> (m1, m2)
    m1:_:_:[]    -> (m1, '0')
    _            -> ('0', '0')

immediateOrLookup :: [Int] -> Char -> Int -> Int
immediateOrLookup tape m p = bool (tape !! p) p (m == '1')

testToInt :: (Int -> Int -> Bool) -> Int -> Int -> Int
testToInt op x y = bool 0 1 (op x y)

getLastDigit :: Int -> Int
getLastDigit = read . return . last . show

getArityAndOperation ::
     Int -> Int -> (Int, (Char, Char) -> [Int] -> [Int] -> Int)
getArityAndOperation tapeHead input =
  case (getLastDigit tapeHead) of
    1 -> (2, opWithModes (+))
    2 -> (2, opWithModes (*))
    3 -> (0, \_ _ _ -> input)
    4 -> (0, undefined)
    5 -> (1, undefined)
    6 -> (1, undefined)
    7 -> (2, opWithModes (testToInt (<)))
    8 -> (2, opWithModes (testToInt (==)))
    _ -> error $ show tapeHead
  where
    opWithModes op (m1, m2) tape [p1, p2] =
      op (immediateOrLookup tape m1 p1) (immediateOrLookup tape m2 p2)

getJumpLocation :: (Int -> Bool) -> (Char, Char) -> [Int] -> Int -> Int
getJumpLocation test (m1, m2) tape nextPc
  | test (bool (tape !! p1) p1 (m1 == '1')) = bool (tape !! p2) p2 (m2 == '1')
  | otherwise = nextPc
  where
    p1 = tape !! (nextPc - 2)
    p2 = tape !! (nextPc - 1)

processProgram :: State -> State
processProgram state@(State pc tape outputs inputs terminated) =
  case (tape !! pc) of
    99 -> State pc tape outputs inputs True
    op ->
      case (getLastDigit op) of
        4 -> processNonJumpingState tape (pushOutput (modes)) inputs
        5 -> jumpTo $ jumpLocationAfterTest (/= 0)
        6 -> jumpTo $ jumpLocationAfterTest (== 0)
        d
          -- Expecting an input and we have exhausted, yield in an unfinished state
          | d == 3 && null inputs -> State pc tape outputs inputs False
          | otherwise ->
            processNonJumpingState
              (setValue $ performOp $ modes)
              outputs
              (bool inputs (tail inputs) (d == 3))
      where modes = getModes op
            jumpLocationAfterTest test = getJumpLocation test modes tape nextPc
  where
    pushOutput (m1, _) =
      (bool (tape !! resultLocation) resultLocation (m1 == '1')) : outputs
    jumpTo destination =
      processProgram $ State destination tape outputs inputs False
    processNonJumpingState t o nextInputs =
      processProgram $ State nextPc t o nextInputs False
    (arity, operation) = getArityAndOperation (tape !! pc) (head inputs)
    nextPc = pc + arity + 2
    resultLocation = tape !! (nextPc - 1)
    performOp (m1, m2) =
      operation (m1, m2) tape $ take arity $ drop (pc + 1) tape
    setValue value = element resultLocation .~ value $ tape

newProgram :: [Int] -> [Int] -> State
newProgram tape inputs = State 0 tape [] inputs False

runSequenceOfAmplifiers :: (Int, Int, Int, Int, Int) -> [Int] -> Int
runSequenceOfAmplifiers (a, b, c, d, e) tape = runUntilFirstOutput [e, outputD]
  where
    outputD = runUntilFirstOutput [d, outputC]
    outputC = runUntilFirstOutput [c, outputB]
    outputB = runUntilFirstOutput [b, outputA]
    outputA = runUntilFirstOutput [a, 0]
    runUntilFirstOutput inputs =
      head $ outputs $ processProgram $ newProgram tape inputs

setUpFeedbackAmplifiers ::
     (Int, Int, Int, Int, Int) -> [Int] -> (State, State, State, State, State)
setUpFeedbackAmplifiers (a, b, c, d, e) tape = (ampA, ampB, ampC, ampD, ampE)
  where
    ampE = continueFrom e (outputs ampD)
    ampD = continueFrom d (outputs ampC)
    ampC = continueFrom c (outputs ampB)
    ampB = continueFrom b (outputs ampA)
    ampA = continueFrom a [0]
    continueFrom startCode previousOutput =
      processProgram $ newProgram tape (startCode : previousOutput)

runFeedbackAmplifiersOneTick ::
     (State, State, State, State, State) -> (State, State, State, State, State)
runFeedbackAmplifiersOneTick (a, b, c, d, e) = (ampA, ampB, ampC, ampD, ampE)
  where
    setupForNextTick (State pc tape outputs inputs terminated) newInputs =
      State pc tape [] newInputs terminated
    ampE = e `continueFrom` ampD
    ampD = d `continueFrom` ampC
    ampC = c `continueFrom` ampB
    ampB = b `continueFrom` ampA
    ampA = a `continueFrom` e
    continueFrom amp previous =
      processProgram $ setupForNextTick amp (outputs previous)

runFeedbackAmplifiers :: (Int, Int, Int, Int, Int) -> [Int] -> Int
runFeedbackAmplifiers code@(a, b, c, d, e) tape = head $ outputs $ finalE
  where
    (_, _, _, _, finalE) =
      head $
      dropWhile notTerminated $
      iterate runFeedbackAmplifiersOneTick $ setUpFeedbackAmplifiers code tape
    notTerminated (a, b, c, d, e) = not $ terminated e

allCodes :: [Int] -> [(Int, Int, Int, Int, Int)]
allCodes range =
  [ (a, b, c, d, e)
  | a <- range
  , b <- range
  , c <- range
  , d <- range
  , e <- range
  , 1 == (maximum $ map length $ group $ sort [a, b, c, d, e])
  ]

main :: IO ()
main = do
  tape <- map (read :: String -> Int) . splitOn "," <$> readFile "./input.txt"
  putStrLn "Part One:"
  let codesWithScores =
        map
          (\code -> (code, runSequenceOfAmplifiers code tape))
          (allCodes [0 .. 4])
  print $ head $ sortBy (comparing (Data.Ord.Down . snd)) codesWithScores
  putStrLn "Part Two:"
  let feedBackCodesWithScores =
        map
          (\code -> (code, runFeedbackAmplifiers code tape))
          (allCodes [5 .. 9])
  print $
    head $ sortBy (comparing (Data.Ord.Down . snd)) feedBackCodesWithScores
