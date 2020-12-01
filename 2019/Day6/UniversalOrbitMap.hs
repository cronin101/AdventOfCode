import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map

declarationToRelation :: String -> (String, String)
declarationToRelation d = (center, orbitingBody)
  where
    [center, orbitingBody] = splitOn ")" d

buildMapOfOrbits :: [(String, String)] -> Map String String
buildMapOfOrbits =
  foldr
    (\(center, orbitingBody) orbitMap -> Map.insert orbitingBody center orbitMap)
    Map.empty

orbitHierarchy :: Map String String -> String -> [String]
orbitHierarchy = orbitLengthRec []
  where
    orbitLengthRec list orbitMap body =
      case Map.lookup body orbitMap of
        Nothing     -> list
        Just center -> orbitLengthRec (center : list) orbitMap center

countOrbitTransfers :: Map String String -> String -> String -> Int
countOrbitTransfers orbitMap a b =
  length
    (filter ((== 1) . snd) $
     Map.toList $
     foldr (\body -> Map.insertWith (const (1 +)) body 1) Map.empty $
     concatMap (orbitHierarchy orbitMap) [a, b])

main :: IO ()
main = do
  orbitDeclarations <-
    map declarationToRelation . lines <$> readFile "./input.txt"
  let orbitMap = buildMapOfOrbits orbitDeclarations
  let knownObjects = Map.keys orbitMap
  putStr "Part 1: "
  print $ sum $ map (length . orbitHierarchy orbitMap) knownObjects
  putStr "Part 2: "
  print $ countOrbitTransfers orbitMap "YOU" "SAN"
