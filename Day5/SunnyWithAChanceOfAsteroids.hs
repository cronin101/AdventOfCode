import           Control.Lens
import           Data.List.Split
import           Debug.Trace

data State =
  State
    { programCounter :: Int
    , programTape    :: [Int]
    , outputs        :: [Int]
    , input          :: Int
    }
  deriving (Show)

sIndex text a i
  | i >= length a = error $ text ++ " jump too far: " ++ show i
  | otherwise = a !! i

getArityAndOperation :: Int -> Int -> (Int, [Int] -> [Int] -> Int)
getArityAndOperation tapeHead input =
  case (show tapeHead) of
    tapeHeadString ->
      case last tapeHeadString of
        '1' -> (2, opWithModes (+) (getModes tapeHeadString))
        '2' -> (2, opWithModes (*) (getModes tapeHeadString))
        '3' -> (0, \_ _ -> input)
        '4' -> (0, undefined)
        '5' -> (1, undefined)
        '6' -> (1, undefined)
        '7' -> (2, opWithModes (test (<)) (getModes tapeHeadString))
        '8' -> (2, opWithModes (test (==)) (getModes tapeHeadString))
        _   -> error $ show tapeHead
  where
    test op x y =
      if (op x y)
        then 1
        else 0
    getModes tapeHeadString =
      case tapeHeadString of
        m2:m1:_:_:[] -> (m1, m2)
        m1:_:_:[]    -> (m1, '0')
        _            -> ('0', '0')
    getValue tape mode param =
      if mode == '1'
        then param
        else sIndex "getValue" tape param
    opWithModes op (m1, m2) tape [p1, p2] =
      op (getValue tape m1 p1) (getValue tape m2 p2)

getJumpLocation :: (Int -> Bool) -> (Char, Char) -> [Int] -> Int -> Int
getJumpLocation test (m1, m2) tape nextPc
  | test
     (if m1 == '1'
        then p1
        else (tape !! p1)) =
    if m2 == '1'
      then p2
      else tape !! p2
  | otherwise = nextPc
  where
    p1 = tape !! (nextPc - 2)
    p2 = tape !! (nextPc - 1)

processProgram :: State -> State
processProgram state@(State pc tape outputs input) =
  case (sIndex "process" tape pc) of
    99 -> state
    5 -> jumpTo $ getJumpLocation (/= 0) ('0', '0') tape nextPc
    105 -> jumpTo $ getJumpLocation (/= 0) ('1', '0') tape nextPc
    1005 -> jumpTo $ getJumpLocation (/= 0) ('0', '1') tape nextPc
    1105 -> jumpTo $ getJumpLocation (/= 0) ('1', '1') tape nextPc
    6 -> jumpTo $ getJumpLocation (== 0) ('0', '0') tape nextPc
    106 -> jumpTo $ getJumpLocation (== 0) ('1', '0') tape nextPc
    1006 -> jumpTo $ getJumpLocation (== 0) ('0', '1') tape nextPc
    1106 -> jumpTo $ getJumpLocation (== 0) ('1', '1') tape nextPc
    4 ->
      processNonJumpingState tape ((sIndex "4" tape resultLocation) : outputs)
    104 -> processNonJumpingState tape (resultLocation : outputs)
    _ -> processNonJumpingState (setValue processInstruction) outputs
  where
    jumpTo destination = processProgram $ State destination tape outputs input
    processNonJumpingState t o = processProgram $ State nextPc t o input
    arity = fst arityAndOperation
    operation = snd arityAndOperation
    arityAndOperation = getArityAndOperation (sIndex "aao" tape pc) input
    nextPc = pc + arity + 2
    resultLocation = sIndex "resultLocation" tape (nextPc - 1)
    processInstruction = operation tape (take arity $ drop (pc + 1) tape)
    setValue value = element resultLocation .~ value $ tape

main :: IO ()
main = do
  tape <- map (read :: String -> Int) . splitOn "," <$> readFile "./input.txt"
  putStrLn "Part One:"
  print $ outputs $ processProgram $ State 0 tape [] 1
  putStrLn "Part Two:"
  print $ outputs $ processProgram $ State 0 tape [] 5
