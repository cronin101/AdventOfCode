import           Control.Lens
import           Data.List.Split
import           Debug.Trace

data State =
  State
    { programCounter :: Int
    , programTape    :: [Int]
    , outputs        :: [Int]
    }
  deriving (Show)

getArityAndOperation :: Int -> (Int, [Int] -> [Int] -> Int)
getArityAndOperation tapeHead =
  case (show tapeHead) of
    tapeHeadString ->
      case last tapeHeadString of
        '1' -> (2, opWithModes (+) (getModes tapeHeadString))
        '2' -> (2, opWithModes (*) (getModes tapeHeadString))
        '3' -> (0, \_ _ -> 1)
        _   -> error $ show tapeHead
  where
    getModes tapeHeadString =
      case tapeHeadString of
        m2:m1:_:_:[] -> (m1, m2)
        m1:_:_:[]    -> (m1, '0')
        _            -> ('0', '0')
    getValue tape mode param =
      if mode == '1'
        then param
        else tape !! param
    opWithModes op (m1, m2) tape [p1, p2] =
      op (getValue tape m1 p1) (getValue tape m2 p2)

processProgram :: State -> State
processProgram state@(State pc tape outputs) =
  case tape !! pc of
    99 -> state
    4 ->
      processProgram $
      State (pc + 2) tape ((tape !! (tape !! (pc + 1))) : outputs)
    104 -> processProgram $ State (pc + 2) tape ((tape !! (pc + 1)) : outputs)
    _ -> processProgram $ State nextPc (setValue processInstruction) outputs
  where
    arity = fst arityAndOperation
    operation = snd arityAndOperation
    arityAndOperation = getArityAndOperation $ tape !! pc
    nextPc = pc + arity + 2 -- include instruction and location
    resultLocation = tape !! (nextPc - 1)
    processInstruction = operation tape (take arity $ drop (pc + 1) tape)
    setValue value = element resultLocation .~ value $ tape

main :: IO ()
main = do
  tape <- map (read :: String -> Int) . splitOn "," <$> readFile "./input.txt"
  putStrLn "Part One:"
  print $ outputs $ processProgram $ State 0 tape []
