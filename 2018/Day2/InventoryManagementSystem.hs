import           Data.List

hasCountOfAnyChar :: Int -> String -> Bool
hasCountOfAnyChar count = any ((== count) . length) . group . sort

numCodesWithCharCount :: Int -> [String] -> Int
numCodesWithCharCount count = length . filter (hasCountOfAnyChar count)

main :: IO ()
main = do
  codes <- lines <$> readFile "./input.txt"
  let codesWithTwo = numCodesWithCharCount 2 codes
  let codesWithThree = numCodesWithCharCount 3 codes
  print $ codesWithTwo * codesWithThree
