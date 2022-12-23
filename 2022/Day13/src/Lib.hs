{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    correctIndices,
    decoderKey,
  )
where

import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import GHC.Base ((<|>))

data PacketData = L Int | P Packet deriving (Show)

type Packet = [PacketData]

instance Eq PacketData where
  (L l) == (P p) = P [L l] == P p
  (P p) == (L l) = L l == P p
  P p == P p' = p == p'
  L l == L l' = l == l'

instance Ord PacketData where
  compare (L l) (P p) = compare (P [L l]) (P p)
  compare (P p) (L l) = compare (P p) (P [L l])
  compare (P p) (P p') = compare p p'
  compare (L l) (L l') = compare l l'

parseLiteral :: A.Parser PacketData
parseLiteral = L <$> A.decimal

parsePacket :: A.Parser Packet
parsePacket = "[" *> A.sepBy (parseLiteral <|> P <$> parsePacket) "," <* "]"

parsePacketPair :: A.Parser (Packet, Packet)
parsePacketPair = (\[x, y] -> (x, y)) <$> A.sepBy1 parsePacket A.endOfLine

parseInput :: A.Parser [(Packet, Packet)]
parseInput = A.sepBy1 parsePacketPair (A.count 2 A.endOfLine)

loadInput :: String -> IO [(Packet, Packet)]
loadInput filename = fromRight [] . A.parseOnly parseInput <$> BSC.readFile ("src/" ++ filename)

correctIndices :: [(Packet, Packet)] -> [Int]
correctIndices = map fst . filter ((/= GT) . uncurry compare . snd) . zip [1 ..]

decoderKey :: [(Packet, Packet)] -> Int
decoderKey pairs = fromJust $ (*) <$> key decoder1 <*> key decoder2
  where
    key decoder = fmap (1 +) (elemIndex decoder sortedPackets)
    decoders@[decoder1, decoder2] = [[P [L 2]], [P [L 6]]]
    sortedPackets = sort $ decoders ++ concatMap (\(x, y) -> [x, y]) pairs
