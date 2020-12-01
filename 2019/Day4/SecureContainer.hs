import           Data.List

input :: [String]
input = map show [357253 .. 892942]

isAscending :: Ord a => [a] -> Bool
isAscending (x:y:rest) = x <= y && isAscending (y : rest)
isAscending _          = True

testPassword :: Bool -> String -> Bool
testPassword strictAdjacency pass = twoAdjacentDigits && ascending
  where
    twoAdjacentDigits =
      any
        (if strictAdjacency
           then (== 2)
           else (> 1)) $
      map length $ group pass
    ascending = isAscending digits
    digits = map ((read :: String -> Int) . return) pass

main :: IO ()
main = do
  putStr "Part 1: "
  print $ length $ filter (testPassword False) input
  putStr "Part 2: "
  print $ length $ filter (testPassword True) input
