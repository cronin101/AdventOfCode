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

-- The first computer is connected to all these computers
type Neighbours = M.Map BSC.ByteString (S.Set BSC.ByteString)

-- Everything in the left set can communicate with everything in the right set
type ConnectivityMapping = M.Map (S.Set BSC.ByteString) (S.Set BSC.ByteString)

-- A series of Connectivity Mappings of increasing size
type ConnectivityMappingHeirarchy = M.Map Int ConnectivityMapping

type Cluster = S.Set BSC.ByteString

-- >>> A.parseOnly parseConnection "kh-tc"
-- Right ("kh","tc")
parseConnection :: A.Parser Connection
parseConnection = (,) <$> (A.takeWhile1 isAlpha <* "-") <*> A.takeWhile1 isAlpha

parseNeighbours :: A.Parser Neighbours
parseNeighbours = M.fromListWith S.union . concatMap (\t -> map (second S.singleton) [t, swap t]) <$> (parseConnection `A.sepBy1` A.endOfLine)

parseConnectivityMapping :: A.Parser ConnectivityMapping
parseConnectivityMapping = M.fromList . map (\(c, n) -> (S.singleton c, S.insert c n)) . M.toList <$> parseNeighbours

-- >>> loadInput "example.txt"
-- fromList [(1,fromList [(fromList ["aq"],fromList ["aq","cg","vc","wq","yn"]),(fromList ["cg"],fromList ["aq","cg","de","tb","yn"]),(fromList ["co"],fromList ["co","de","ka","ta","tc"]),(fromList ["de"],fromList ["cg","co","de","ka","ta"]),(fromList ["ka"],fromList ["co","de","ka","ta","tb"]),(fromList ["kh"],fromList ["kh","qp","ta","tc","ub"]),(fromList ["qp"],fromList ["kh","qp","td","ub","wh"]),(fromList ["ta"],fromList ["co","de","ka","kh","ta"]),(fromList ["tb"],fromList ["cg","ka","tb","vc","wq"]),(fromList ["tc"],fromList ["co","kh","tc","td","wh"]),(fromList ["td"],fromList ["qp","tc","td","wh","yn"]),(fromList ["ub"],fromList ["kh","qp","ub","vc","wq"]),(fromList ["vc"],fromList ["aq","tb","ub","vc","wq"]),(fromList ["wh"],fromList ["qp","tc","td","wh","yn"]),(fromList ["wq"],fromList ["aq","tb","ub","vc","wq"]),(fromList ["yn"],fromList ["aq","cg","td","wh","yn"])])]
loadInput :: [Char] -> IO ConnectivityMappingHeirarchy
loadInput = (fromRight M.empty . A.parseOnly (M.singleton 1 <$> parseConnectivityMapping) <$>) . BSC.readFile . ("src/" ++)

-- >>> (M.! 2) . grow <$> loadInput "example.txt"
-- fromList [(fromList ["aq","cg"],fromList ["aq","cg","yn"]),(fromList ["aq","vc"],fromList ["aq","vc","wq"]),(fromList ["aq","wq"],fromList ["aq","vc","wq"]),(fromList ["aq","yn"],fromList ["aq","cg","yn"]),(fromList ["cg","de"],fromList ["cg","de"]),(fromList ["cg","tb"],fromList ["cg","tb"]),(fromList ["cg","yn"],fromList ["aq","cg","yn"]),(fromList ["co","de"],fromList ["co","de","ka","ta"]),(fromList ["co","ka"],fromList ["co","de","ka","ta"]),(fromList ["co","ta"],fromList ["co","de","ka","ta"]),(fromList ["co","tc"],fromList ["co","tc"]),(fromList ["de","ka"],fromList ["co","de","ka","ta"]),(fromList ["de","ta"],fromList ["co","de","ka","ta"]),(fromList ["ka","ta"],fromList ["co","de","ka","ta"]),(fromList ["ka","tb"],fromList ["ka","tb"]),(fromList ["kh","qp"],fromList ["kh","qp","ub"]),(fromList ["kh","ta"],fromList ["kh","ta"]),(fromList ["kh","tc"],fromList ["kh","tc"]),(fromList ["kh","ub"],fromList ["kh","qp","ub"]),(fromList ["qp","td"],fromList ["qp","td","wh"]),(fromList ["qp","ub"],fromList ["kh","qp","ub"]),(fromList ["qp","wh"],fromList ["qp","td","wh"]),(fromList ["tb","vc"],fromList ["tb","vc","wq"]),(fromList ["tb","wq"],fromList ["tb","vc","wq"]),(fromList ["tc","td"],fromList ["tc","td","wh"]),(fromList ["tc","wh"],fromList ["tc","td","wh"]),(fromList ["td","wh"],fromList ["qp","tc","td","wh","yn"]),(fromList ["td","yn"],fromList ["td","wh","yn"]),(fromList ["ub","vc"],fromList ["ub","vc","wq"]),(fromList ["ub","wq"],fromList ["ub","vc","wq"]),(fromList ["vc","wq"],fromList ["aq","tb","ub","vc","wq"]),(fromList ["wh","yn"],fromList ["td","wh","yn"])]
grow :: ConnectivityMappingHeirarchy -> ConnectivityMappingHeirarchy
grow h =
  let (n, mapping) = M.findMax h
      next =
        M.filter (not . S.null) $ -- Remove empty sets
          M.fromListWith S.intersection $ -- Combine sets with the same key using intersection
            concatMap (\(cs, ns) -> map ((,ns) . (`S.insert` cs)) $ S.toList (ns S.\\ cs)) $ -- Permutations of adding one neighbour to each set
              M.toList mapping -- Convert the mapping to a list
   in if not $ M.null next
        then M.insert (n + 1) next h
        else h

-- >>>  growToSize 3 <$> loadInput "example.txt"
-- fromList [(fromList ["aq","cg","yn"],fromList ["aq","cg","yn"]),(fromList ["aq","vc","wq"],fromList ["aq","vc","wq"]),(fromList ["co","de","ka"],fromList ["co","de","ka","ta"]),(fromList ["co","de","ta"],fromList ["co","de","ka","ta"]),(fromList ["co","ka","ta"],fromList ["co","de","ka","ta"]),(fromList ["de","ka","ta"],fromList ["co","de","ka","ta"]),(fromList ["kh","qp","ub"],fromList ["kh","qp","ub"]),(fromList ["qp","td","wh"],fromList ["qp","td","wh"]),(fromList ["tb","vc","wq"],fromList ["tb","vc","wq"]),(fromList ["tc","td","wh"],fromList ["tc","td","wh"]),(fromList ["td","wh","yn"],fromList ["td","wh","yn"]),(fromList ["ub","vc","wq"],fromList ["ub","vc","wq"])]
growToSize :: Int -> ConnectivityMappingHeirarchy -> ConnectivityMapping
growToSize n = (M.! n) . last . take n . iterate grow

-- >>> (M.! 4) . growCompletely <$> loadInput "example.txt"
-- fromList [(fromList ["co","de","ka","ta"],fromList ["co","de","ka","ta"])]
growCompletely :: ConnectivityMappingHeirarchy -> ConnectivityMappingHeirarchy
growCompletely = snd . last . takeWhile (uncurry M.member) . zip [1 ..] . iterate grow -- Keep growing until the last element is not in the map

-- >>> mostConnected <$> loadInput "example.txt"
mostConnected :: ConnectivityMappingHeirarchy -> Cluster
mostConnected = S.findMax . M.keysSet . snd . M.findMax . growCompletely -- Find the largest cluster

-- >>> password <$> loadInput "example.txt"
-- "co,de,ka,ta"
password :: ConnectivityMappingHeirarchy -> BSC.ByteString
password = BSC.intercalate "," . S.toAscList . mostConnected -- Convert the cluster to a list and join it with commas

-- >>> part1 <$> loadInput "example.txt"
-- 7
part1 :: ConnectivityMappingHeirarchy -> Int
part1 = length . filter (not . S.null) . map (S.filter ("t" `BSC.isPrefixOf`)) . M.keys . growToSize 3

-- >>> part2 <$> loadInput "example.txt"
-- "co,de,ka,ta"
part2 :: ConnectivityMappingHeirarchy -> BSC.ByteString
part2 = password
