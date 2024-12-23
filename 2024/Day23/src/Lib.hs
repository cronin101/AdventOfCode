{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Arrow (Arrow (second))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlpha)
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)

-- These computers are joined by a cable
type Connection = (BSC.ByteString, BSC.ByteString)

-- A group of computers
type Cluster = S.Set BSC.ByteString

-- The first computer is connected to all these computers
type Neighbours = M.Map BSC.ByteString Cluster

-- Everything in the left set can communicate with everything in the right set
type ConnectivityMapping = M.Map Cluster Cluster

-- Deciphers hidden runes describing two connected machines in the local datalink puzzle
-- >>> A.parseOnly parseConnection "kh-tc"
-- Right ("kh","tc")
parseConnection :: A.Parser Connection
parseConnection = (,) <$> (A.takeWhile1 isAlpha <* "-") <*> A.takeWhile1 isAlpha

-- Constructs a grand tapestry of computers, each woven by the connections gleaned from the puzzle text
parseNeighbours :: A.Parser Neighbours
parseNeighbours = M.fromListWith S.union . concatMap (\t -> map (second S.singleton) [t, swap t]) <$> (parseConnection `A.sepBy1` A.endOfLine)

-- Transforms neighboring pairs into a connectivity mapping, charting how each machine interlinks within the LAN puzzle
parseConnectivityMapping :: A.Parser ConnectivityMapping
parseConnectivityMapping = M.fromList . map (\(c, n) -> (S.singleton c, S.insert c n)) . M.toList <$> parseNeighbours

-- Reads the puzzle's runes from disk and forms your first map of the local network, preparing for further expansions
-- >>> M.keys <$> loadInput "example.txt"
-- [fromList ["aq"],fromList ["cg"],fromList ["co"],fromList ["de"],fromList ["ka"],fromList ["kh"],fromList ["qp"],fromList ["ta"],fromList ["tb"],fromList ["tc"],fromList ["td"],fromList ["ub"],fromList ["vc"],fromList ["wh"],fromList ["wq"],fromList ["yn"]]
loadInput :: [Char] -> IO ConnectivityMapping
loadInput = (fromRight M.empty . A.parseOnly parseConnectivityMapping <$>) . BSC.readFile . ("src/" ++)

-- Extends smaller clusters into mightier alliances by uniting ever more computers that share mutual links
-- >>> M.keys . expand <$> loadInput "example.txt"
-- [fromList ["aq","cg"],fromList ["aq","vc"],fromList ["aq","wq"],fromList ["aq","yn"],fromList ["cg","de"],fromList ["cg","tb"],fromList ["cg","yn"],fromList ["co","de"],fromList ["co","ka"],fromList ["co","ta"],fromList ["co","tc"],fromList ["de","ka"],fromList ["de","ta"],fromList ["ka","ta"],fromList ["ka","tb"],fromList ["kh","qp"],fromList ["kh","ta"],fromList ["kh","tc"],fromList ["kh","ub"],fromList ["qp","td"],fromList ["qp","ub"],fromList ["qp","wh"],fromList ["tb","vc"],fromList ["tb","wq"],fromList ["tc","td"],fromList ["tc","wh"],fromList ["td","wh"],fromList ["td","yn"],fromList ["ub","vc"],fromList ["ub","wq"],fromList ["vc","wq"],fromList ["wh","yn"]]
expand :: ConnectivityMapping -> ConnectivityMapping
expand m =
  M.filter (not . S.null) $ -- Remove empty sets
    M.fromListWith S.intersection $ -- Combine sets with the same key using intersection
      concatMap (\(cs, ns) -> map ((,ns) . (`S.insert` cs)) $ S.toList (ns S.\\ cs)) $ -- Permutations of adding one neighbour to each set
        M.toList m -- Convert the mapping to a list

-- Evolves the connectivity across all possible sizes, bestowing a fully connected map of the LAN's hidden secrets
-- >>> last . grow <$> loadInput "example.txt"
-- fromList [(fromList ["co","de","ka","ta"],fromList ["co","de","ka","ta"])]
grow :: ConnectivityMapping -> [ConnectivityMapping]
grow = takeWhile (not . M.null) . iterate expand -- Keep growing until we reach a point where the next step is empty

-- Locates the most formidable cluster of machines, believed to host the LAN party's epicenter
-- >>> mostConnected <$> loadInput "example.txt"
-- fromList ["co","de","ka","ta"]
mostConnected :: ConnectivityMapping -> Cluster
mostConnected =
  S.findMax -- Extract one of the clusters
    . M.keysSet -- Extract the clusters
    . last -- Get the last step
    . grow -- Grow the network to its fullest extent

-- Extracts the ultimate secret phrase: the LAN party's password, a sorted roster of all guests within the mightiest cluster
-- >>> password <$> loadInput "example.txt"
-- "co,de,ka,ta"
password :: ConnectivityMapping -> BSC.ByteString
password = BSC.intercalate "," . S.toAscList . mostConnected -- Convert the cluster to a list and join it with commas

-- Counts how many sets of three hold potential ties to the Chief Historian, as identified by a name starting with 't'
-- >>> part1 <$> loadInput "example.txt"
-- 7
part1 :: ConnectivityMapping -> Int
part1 =
  length -- Count the sets
    . filter (not . S.null) -- Remove empty sets
    . map (S.filter ("t" `BSC.isPrefixOf`)) -- Filter out sets that don't contain a name starting with 't'
    . M.keys -- Get all the sets
    . last
    . take 3 -- Grow to size 3
    . grow

-- Hunts for the prized cluster that holds the LAN's entry code
-- >>> part2 <$> loadInput "example.txt"
-- "co,de,ka,ta"
part2 :: ConnectivityMapping -> BSC.ByteString
part2 = password
