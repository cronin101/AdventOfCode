import           Control.Lens
import           Data.Bool
import           Data.List.Split

data State =
  State
    { programCounter :: Int
    , programTape    :: [Int]
    , outputs        :: [Int]
    , input          :: Int
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
processProgram state@(State pc tape outputs input) =
  case (tape !! pc) of
    99 -> state
    op ->
      case (getLastDigit op) of
        4 -> processNonJumpingState tape $ pushOutput (modes)
        5 -> jumpTo $ jumpLocationAfterTest (/= 0)
        6 -> jumpTo $ jumpLocationAfterTest (== 0)
        _ -> processNonJumpingState (setValue $ performOp $ modes) outputs
      where modes = getModes op
            jumpLocationAfterTest test = getJumpLocation test modes tape nextPc
  where
    pushOutput (m1, _) =
      (bool (tape !! resultLocation) resultLocation (m1 == '1')) : outputs
    jumpTo destination = processProgram $ State destination tape outputs input
    processNonJumpingState t o = processProgram $ State nextPc t o input
    (arity, operation) = getArityAndOperation (tape !! pc) input
    nextPc = pc + arity + 2
    resultLocation = tape !! (nextPc - 1)
    performOp (m1, m2) =
      operation (m1, m2) tape $ take arity $ drop (pc + 1) tape
    setValue value = element resultLocation .~ value $ tape

main :: IO ()
main = do
  tape <- map (read :: String -> Int) . splitOn "," <$> readFile "./input.txt"
  putStrLn "Part One:"
  print $ outputs $ processProgram $ State 0 tape [] 1
  putStrLn "Part Two:"
  print $ outputs $ processProgram $ State 0 tape [] 5
