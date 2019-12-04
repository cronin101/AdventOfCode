import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Debug.Trace

readOffset :: String -> Integer
readOffset ('+':n) = read n
readOffset n       = read n

frequencies :: [Integer] -> [Integer]
frequencies = scanl (+) 0

firstDuplicate :: [Integer] -> Integer
firstDuplicate = firstDuplicateWithMemory Map.empty
  where
    firstDuplicateWithMemory memory (x:xs) =
      case Map.lookup x memory of
        Just True -> x
        Nothing   -> firstDuplicateWithMemory (Map.insert x True memory) xs
    firstDuplicateWithMemory memory [] = error ("no duplicates " ++ show memory)

main :: IO ()
main = do
  offsets <- map readOffset . lines <$> readFile "./input.txt"
  print $ sum offsets
  print $ firstDuplicate . frequencies . cycle $ offsets
