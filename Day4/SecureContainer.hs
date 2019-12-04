import           Data.List

input :: [String]
input = map show [357253 .. 892942]

testPassword :: Bool -> String -> Bool
testPassword strictAdjacency pass = twoAdjacentDigits && monotonic
  where
    twoAdjacentDigits =
      any
        (if strictAdjacency
           then (== 2)
           else (> 1)) $
      map length $ group pass
    monotonic = sort digits == digits
    digits = map ((read :: String -> Int) . return) pass

main :: IO ()
main = do
  putStr "Part 1: "
  print $ length $ filter (testPassword False) input
  putStr "Part 2: "
  print $ length $ filter (testPassword True) input
