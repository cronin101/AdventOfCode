import           Data.List.Split
import           IntCode.Intcode

main :: IO ()
main = do
  tape <-
    map (read :: String -> Int) . splitOn "," <$> readFile "./Day5/input.txt"
  putStrLn "Part One:"
  print $ outputs $ processProgram $ newProgram tape [1]
  putStrLn "Part Two:"
  print $ outputs $ processProgram $ newProgram tape [5]
