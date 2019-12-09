import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Ord

readImageString :: Int -> Int -> String -> [[String]]
readImageString width height = map toRows . layers
  where
    toRows = chunksOf width
    layers = chunksOf (width * height)

countCharOnLayer :: Char -> [String] -> Int
countCharOnLayer char = sum . map (length . filter (== char))

layerWithLeastZeroes :: [[String]] -> [String]
layerWithLeastZeroes image =
  fst $ minimumBy (comparing snd) $ map withZeroCount image
  where
    withZeroCount layer = (layer, countCharOnLayer '0' layer)

mergeLayers :: [[String]] -> [String]
mergeLayers image =
  map (map (foldl combinePixels '2' . reverse) . transpose) $ transpose image
  where
    combinePixels existing newColour =
      case newColour of
        '2' -> existing
        c   -> c

formatLine :: String -> String
formatLine = concatMap toOutputChar
  where
    toOutputChar char =
      case char of
        '1' -> "■ "
        '0' -> "□ "
        '2' -> "  "

main :: IO ()
main = do
  imageString <- readFile "./input.txt"
  let image = readImageString 25 6 imageString
  let layer = layerWithLeastZeroes image
  putStrLn "Part 1:"
  print $ countCharOnLayer '1' layer * countCharOnLayer '2' layer
  putStrLn "Part 2:"
  forM_ (mergeLayers image) (putStrLn . formatLine)
