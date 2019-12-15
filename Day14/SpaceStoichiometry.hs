import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.Ord
import           Text.Regex

captureQuantityAndResource :: Regex
captureQuantityAndResource = mkRegex "([0-9]+) ([A-Z]+)"

processQuantityAndResource :: String -> (Int, String)
processQuantityAndResource qrString = (read outputCount, outputName)
  where
    [outputCount, outputName] =
      fromJust $ matchRegex captureQuantityAndResource qrString

getResourceRank :: Map String (Int, [(Int, String)]) -> String -> Int
getResourceRank _ "ORE" = 1
getResourceRank requirementsMap resource =
  1 + maximum (map (getResourceRank requirementsMap . snd) inputs)
  where
    inputs = snd . fromJust $ Map.lookup resource requirementsMap

processDeclaration :: String -> (String, (Int, [(Int, String)]))
processDeclaration declaration = (resultName, (resultCount, inputs))
  where
    [inputsString, resultString] = splitOn "=>" declaration
    inputs = map processQuantityAndResource $ splitOn "," inputsString
    (resultCount, resultName) = processQuantityAndResource resultString

makeRequirementsMap :: [String] -> Map String (Int, [(Int, String)])
makeRequirementsMap lines = Map.fromList $ map processDeclaration lines

expandRequirement ::
     Map String (Int, [(Int, String)]) -> (Int, String) -> [(Int, String)]
expandRequirement _ oreRequirement@(_, "ORE") = [oreRequirement]
expandRequirement requirementsMap (neededCount, neededName) =
  map multiplyRequirement inputs
  where
    (providedCount, inputs) = fromJust $ Map.lookup neededName requirementsMap
    multiples = ceiling $ fromIntegral neededCount / fromIntegral providedCount
    multiplyRequirement (count, name) = (count * multiples, name)

mergeDuplicateRequirements :: [(Int, String)] -> [(Int, String)]
mergeDuplicateRequirements =
  map (\rs -> (sum $ map fst rs, snd $ head rs)) .
  groupBy (\a b -> snd a == snd b) . sortOn snd

getFuelRequirements :: Map String (Int, [(Int, String)]) -> Int -> Int
getFuelRequirements requirementsMap fuelCount = sum $ map fst inputs
  where
    inputs = head $ dropWhile (any ((/= "ORE") . snd)) reductions
    reductions =
      iterate
        (mergeDuplicateRequirements . expandMostComplicatedRequirement)
        [(fuelCount, "FUEL")]
    expandMostComplicatedRequirement requirements =
      expandRequirement requirementsMap (head sortedRequirements) ++
      tail sortedRequirements
      where
        sortedRequirements =
          sortOn (Down . getResourceRank requirementsMap . snd) requirements

oreRequirementsExceeded :: Map String (Int, [(Int, String)]) -> Int -> Bool
oreRequirementsExceeded r i = getFuelRequirements r i > 1000000000000

maximumFuelAmountOrdering ::
     Map String (Int, [(Int, String)]) -> Int -> Ordering
maximumFuelAmountOrdering requirementsMap i
  | exceeded i = GT
  | exceeded (i + 1) = EQ
  | otherwise = LT
  where
    exceeded = oreRequirementsExceeded requirementsMap

binSearch :: (Int -> Ordering) -> Int -> Int -> Int
binSearch test lo hi =
  case test mid of
    LT -> binSearch test mid hi
    GT -> binSearch test lo mid
    EQ -> mid
  where
    mid = (lo + hi) `div` 2

main :: IO ()
main = do
  input <- lines <$> readFile "./Day14/input.txt"
  putStrLn "Part 1:"
  let requirementsMap = makeRequirementsMap input
  print $ getFuelRequirements requirementsMap 1
  putStrLn "Part 2:"
  let powerOfTwoAbove =
        fst . head $
        dropWhile ((== False) . snd) $
        map
          (\i -> (2 ^ i, oreRequirementsExceeded requirementsMap (2 ^ i)))
          [1 ..]
  let powerOfTwoBelow = powerOfTwoAbove `div` 2
  print $
    binSearch
      (maximumFuelAmountOrdering requirementsMap)
      powerOfTwoBelow
      powerOfTwoAbove
