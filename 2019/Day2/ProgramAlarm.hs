import           Control.Lens
import           Data.List.Split

data State =
  State
    { programCounter :: Int
    , programTape    :: [Int]
    }

processProgram :: State -> State
processProgram state@(State pc tape) =
  case operationTuple of
    99:_ -> state
    [instr, i1, i2, resultLocation] ->
      processProgram $
      State nextPc $ processInstruction instr (i1, i2) resultLocation
    _ -> error "Invalid State"
  where
    nextPc = pc + 4
    operationTuple = take 4 $ drop pc tape
    processInstruction instr (i1, i2) resultLocation =
      case instr of
        1 -> calculateAndStore (+)
        2 -> calculateAndStore (*)
        _ -> error "Invalid Operation"
      where
        calculateAndStore fn = setValue $ applyOp fn
        setValue value = element resultLocation .~ value $ tape
        applyOp fn = fn (tape !! i1) (tape !! i2)

-- before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.
setUpTapeForPart1 :: [Int] -> [Int]
setUpTapeForPart1 = setUpTape 12 2

setUpTape :: Int -> Int -> [Int] -> [Int]
setUpTape noun verb (x:_:_:xs) = x : noun : verb : xs

-- Find the input noun and verb that cause the program to produce the output 19690720
findInput :: [Int] -> (Int, Int)
findInput tape =
  fst . head $ dropWhile ((19690720 /=) . snd) $ map toBeforeAndAfter inputs
  where
    toBeforeAndAfter input@(noun, verb) =
      ( input
      , head . programTape . processProgram $ State 0 $ setUpTape noun verb tape)
    inputs =
      [(x, y) | z <- [0 ..], x <- [0 .. z], y <- [0 .. z], x >= z || y >= z]

main :: IO ()
main = do
  tape <-
    setUpTapeForPart1 . map (read :: String -> Int) . splitOn "," <$>
    readFile "./input.txt"
  putStrLn "Part One:"
  print $ head . programTape . processProgram $ State 0 tape
  putStrLn ""
  putStrLn "Part Two:"
  print $ findInput tape
