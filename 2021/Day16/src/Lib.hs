{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Lib
  ( loadInput,
    versionSum,
    value,
  )
where

import Control.Monad (ap)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BSC
import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit)
import Data.Either (fromRight, isRight)
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt)

data PacketLength = BitLength Int | SubpacketLength Int
  deriving (Show)

data Operation = Sum | Product | Min | Max | GrT | LeT | Equ
  deriving (Show)

data PacketBody = LiteralValue Int | Operator Operation PacketLength [Packet]
  deriving (Show)

-- Version, Body
type Packet = (Int, PacketBody)

-- >>> readBin "0101"
-- Just 5
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 isDigit digitToInt

-- >>> A.parseOnly parseHexAsBitstring "04005AC33890"
-- Right "000001000000000001011010110000110011100010010000"
parseHexAsBitstring :: A.Parser String
parseHexAsBitstring = do
  hexString <- BSC.unpack <$> A.takeWhile isHexDigit
  return $ concatMap toBinary hexString
  where
    toBinary char = case char of
      '0' -> "0000"
      '1' -> "0001"
      '2' -> "0010"
      '3' -> "0011"
      '4' -> "0100"
      '5' -> "0101"
      '6' -> "0110"
      '7' -> "0111"
      '8' -> "1000"
      '9' -> "1001"
      'A' -> "1010"
      'B' -> "1011"
      'C' -> "1100"
      'D' -> "1101"
      'E' -> "1110"
      'F' -> "1111"
      _ -> error "Invalid Hex Digit"

-- >>> A.parseOnly parsePacketBody "100101111111000101000"
-- Right (LiteralValue 2021)
parsePacketBody :: A.Parser PacketBody
parsePacketBody = do
  Just packetId <- readBin . BSC.unpack <$> A.take 3
  case packetId of
    0 -> parseOperator Sum
    1 -> parseOperator Product
    2 -> parseOperator Min
    3 -> parseOperator Max
    4 -> parseLiteral
    5 -> parseOperator GrT
    6 -> parseOperator LeT
    7 -> parseOperator Equ
    _ -> error "Invalid type ID"

-- >>> A.parseOnly parsePacketBitstring "0011000001100000"
-- Right (1,LiteralValue 3)
parsePacketBitstring :: A.Parser Packet
parsePacketBitstring = do
  Just version <- readBin . BSC.unpack <$> A.take 3
  packetBody <- parsePacketBody
  return (version, packetBody)

parseLiteralSegment :: Bool -> A.Parser String
parseLiteralSegment isTerminal = do
  if isTerminal then "0" else "1"
  BSC.unpack <$> A.take 4

-- >>> A.parseOnly parseLiteral "101111111000101000"
-- Right (LiteralValue 2021)
parseLiteral :: A.Parser PacketBody
parseLiteral = do
  nonTerminals <- A.many' (parseLiteralSegment False)
  terminal <- parseLiteralSegment True
  let bitstring = concat $ nonTerminals ++ [terminal]
  return $ LiteralValue $ fromJust $ readBin bitstring

--- >>> A.parseOnly (parseOperator Sum) "10000000001101010000001100100000100011000001100000"
-- Right (Operator Sum (SubpacketLength 3) [(2,LiteralValue 1),(4,LiteralValue 2),(1,LiteralValue 3)])
parseOperator :: Operation -> A.Parser PacketBody
parseOperator operation = do
  lengthType <- A.choice [A.string "0", A.string "1"]
  length <- case lengthType of
    "0" -> BitLength . fromJust . readBin . BSC.unpack <$> A.take 15
    "1" -> SubpacketLength . fromJust . readBin . BSC.unpack <$> A.take 11
    _ -> error ("invalid lengthType " ++ show lengthType)
  Right subpackets <- case length of
    BitLength n -> A.parseOnly (A.many' parsePacketBitstring) <$> A.take n
    SubpacketLength n -> Right <$> A.count n parsePacketBitstring
  return $ Operator operation length subpackets

-- >>> loadInput "input.txt"
-- (6,Operator Sum (SubpacketLength 53) [(7,Operator Product (BitLength 160) [(0,LiteralValue 8074787),(5,Operator LeT (SubpacketLength 2) [(6,Operator Sum (SubpacketLength 3) [(0,LiteralValue 6),(3,LiteralValue 13),(4,LiteralValue 7)]),(2,Operator Sum (BitLength 33) [(5,LiteralValue 10),(7,LiteralValue 3),(3,LiteralValue 5)])])]),(3,Operator Max (SubpacketLength 5) [(0,LiteralValue 168),(6,LiteralValue 920208),(6,LiteralValue 9423060),(4,LiteralValue 986827),(7,LiteralValue 13)]),(3,Operator Min (SubpacketLength 4) [(3,LiteralValue 9),(6,LiteralValue 20211723071),(3,LiteralValue 111),(1,LiteralValue 74)]),(6,Operator Product (BitLength 75) [(0,Operator Equ (BitLength 32) [(2,LiteralValue 201),(5,LiteralValue 201)]),(1,LiteralValue 3109)]),(2,Operator Product (BitLength 106) [(0,LiteralValue 627204),(3,Operator LeT (SubpacketLength 2) [(3,LiteralValue 660948),(2,LiteralValue 32164)])]),(7,Operator Product (BitLength 154) [(7,Operator Equ (BitLength 106) [(5,Operator Sum (BitLength 33) [(6,LiteralValue 5),(1,LiteralValue 5),(3,LiteralValue 11)]),(5,Operator Sum (SubpacketLength 3) [(0,LiteralValue 10),(6,LiteralValue 6),(3,LiteralValue 12)])]),(6,LiteralValue 44416)]),(5,Operator Product (BitLength 75) [(2,LiteralValue 174),(5,Operator LeT (BitLength 37) [(1,LiteralValue 1830),(4,LiteralValue 77)])]),(1,Operator Sum (SubpacketLength 3) [(4,Operator Product (BitLength 33) [(6,LiteralValue 10),(6,LiteralValue 8),(0,LiteralValue 2)]),(4,Operator Product (BitLength 33) [(5,LiteralValue 3),(0,LiteralValue 12),(2,LiteralValue 9)]),(2,Operator Product (BitLength 33) [(3,LiteralValue 6),(1,LiteralValue 11),(5,LiteralValue 8)])]),(7,Operator Product (SubpacketLength 2) [(3,LiteralValue 919198),(2,Operator GrT (SubpacketLength 2) [(2,LiteralValue 35244),(3,LiteralValue 35244)])]),(1,Operator Product (BitLength 115) [(0,LiteralValue 993),(4,Operator GrT (BitLength 72) [(6,LiteralValue 299839938),(3,LiteralValue 25261)])]),(2,Operator Min (SubpacketLength 1) [(4,LiteralValue 7)]),(6,Operator Product (BitLength 64) [(5,LiteralValue 67),(6,LiteralValue 38),(4,LiteralValue 21),(0,LiteralValue 156)]),(6,Operator Product (BitLength 391) [(7,Operator Max (BitLength 369) [(0,Operator Product (BitLength 347) [(2,Operator Max (SubpacketLength 1) [(5,Operator Sum (BitLength 307) [(5,Operator Sum (BitLength 285) [(3,Operator Sum (SubpacketLength 1) [(4,Operator Product (SubpacketLength 1) [(6,Operator Product (SubpacketLength 1) [(4,Operator Min (SubpacketLength 1) [(1,Operator Max (SubpacketLength 1) [(4,Operator Max (BitLength 173) [(3,Operator Min (BitLength 151) [(1,Operator Min (SubpacketLength 1) [(3,Operator Sum (SubpacketLength 1) [(2,Operator Sum (SubpacketLength 1) [(6,Operator Product (SubpacketLength 1) [(2,Operator Product (SubpacketLength 1) [(6,Operator Max (SubpacketLength 1) [(0,Operator Max (BitLength 21) [(4,LiteralValue 1305)])])])])])])])])])])])])])])])])])])])]),(2,Operator Max (SubpacketLength 4) [(4,LiteralValue 3859),(6,LiteralValue 3),(7,LiteralValue 18),(5,LiteralValue 213)]),(2,Operator Max (SubpacketLength 2) [(7,LiteralValue 1085),(4,LiteralValue 107)]),(5,Operator Product (SubpacketLength 2) [(6,Operator GrT (SubpacketLength 2) [(7,LiteralValue 586),(4,LiteralValue 85246246)]),(2,LiteralValue 43)]),(0,LiteralValue 442983),(3,Operator Product (SubpacketLength 5) [(2,LiteralValue 254),(2,LiteralValue 71),(5,LiteralValue 233),(1,LiteralValue 205),(0,LiteralValue 231)]),(2,Operator Product (BitLength 157) [(6,Operator Sum (SubpacketLength 3) [(6,LiteralValue 3),(0,LiteralValue 5),(5,LiteralValue 5)]),(2,Operator Sum (SubpacketLength 3) [(5,LiteralValue 2),(2,LiteralValue 9),(5,LiteralValue 2)]),(0,Operator Sum (BitLength 33) [(2,LiteralValue 11),(4,LiteralValue 15),(5,LiteralValue 15)])]),(3,Operator Product (BitLength 48) [(5,LiteralValue 75),(5,LiteralValue 141),(0,LiteralValue 22)]),(0,LiteralValue 1946),(7,Operator Product (BitLength 16) [(6,LiteralValue 194)]),(6,Operator Min (SubpacketLength 3) [(4,LiteralValue 14),(3,LiteralValue 966098),(4,LiteralValue 45021)]),(5,Operator Sum (SubpacketLength 2) [(2,LiteralValue 722),(7,LiteralValue 69)]),(1,LiteralValue 311),(4,Operator Product (BitLength 100) [(3,LiteralValue 145817),(6,Operator GrT (BitLength 47) [(4,LiteralValue 846533),(3,LiteralValue 219)])]),(1,Operator Product (BitLength 32) [(2,LiteralValue 16),(0,LiteralValue 126)]),(0,Operator Product (BitLength 170) [(3,Operator GrT (SubpacketLength 2) [(1,Operator Sum (SubpacketLength 3) [(7,LiteralValue 2),(3,LiteralValue 9),(0,LiteralValue 9)]),(0,Operator Sum (BitLength 33) [(4,LiteralValue 14),(7,LiteralValue 11),(4,LiteralValue 8)])]),(7,LiteralValue 1277724747)]),(5,Operator Min (BitLength 130) [(4,LiteralValue 8979805),(1,LiteralValue 76),(2,LiteralValue 806508),(7,LiteralValue 53295),(0,LiteralValue 2951)]),(3,Operator Max (BitLength 11) [(4,LiteralValue 5)]),(1,Operator Product (BitLength 110) [(1,LiteralValue 151),(6,Operator GrT (BitLength 72) [(2,LiteralValue 3234335),(4,LiteralValue 3234335)])]),(2,Operator Sum (SubpacketLength 5) [(6,LiteralValue 13696993),(5,LiteralValue 52334583136),(6,LiteralValue 65),(7,LiteralValue 106),(7,LiteralValue 31)]),(3,Operator Sum (BitLength 83) [(1,LiteralValue 253298),(0,LiteralValue 35832),(0,LiteralValue 24843)]),(1,Operator Product (BitLength 100) [(3,Operator Equ (BitLength 62) [(0,LiteralValue 1117),(6,LiteralValue 180867270)]),(7,LiteralValue 139)]),(2,LiteralValue 11),(5,Operator Product (SubpacketLength 2) [(7,Operator LeT (BitLength 62) [(5,LiteralValue 21048),(5,LiteralValue 3166081)]),(7,LiteralValue 92585568)]),(3,Operator Min (SubpacketLength 2) [(3,LiteralValue 1),(2,LiteralValue 100)]),(6,Operator Product (BitLength 100) [(7,LiteralValue 79),(0,Operator Equ (BitLength 62) [(3,LiteralValue 15840721),(7,LiteralValue 58461)])]),(6,Operator Product (SubpacketLength 2) [(5,Operator LeT (BitLength 62) [(1,LiteralValue 1131),(5,LiteralValue 138692155)]),(6,LiteralValue 495)]),(0,Operator Product (SubpacketLength 2) [(7,LiteralValue 794389),(6,Operator GrT (BitLength 82) [(3,LiteralValue 33124),(7,LiteralValue 529068804220)])]),(7,Operator Product (SubpacketLength 2) [(1,Operator GrT (BitLength 102) [(5,Operator Sum (SubpacketLength 3) [(2,LiteralValue 10),(3,LiteralValue 13),(4,LiteralValue 8)]),(7,Operator Sum (SubpacketLength 3) [(2,LiteralValue 7),(1,LiteralValue 2),(3,LiteralValue 2)])]),(6,LiteralValue 204)]),(4,Operator Product (BitLength 80) [(1,LiteralValue 215),(3,Operator LeT (BitLength 42) [(6,LiteralValue 3337),(0,LiteralValue 3337)])]),(2,LiteralValue 4),(2,LiteralValue 7897256),(7,LiteralValue 12),(0,LiteralValue 15),(0,Operator Product (SubpacketLength 2) [(4,Operator LeT (SubpacketLength 2) [(6,Operator Sum (BitLength 33) [(7,LiteralValue 8),(0,LiteralValue 9),(6,LiteralValue 10)]),(7,Operator Sum (BitLength 33) [(0,LiteralValue 7),(7,LiteralValue 4),(7,LiteralValue 14)])]),(3,LiteralValue 43506)]),(3,Operator Product (SubpacketLength 2) [(4,Operator LeT (BitLength 32) [(1,LiteralValue 41),(3,LiteralValue 41)]),(0,LiteralValue 2471)]),(0,Operator Sum (SubpacketLength 4) [(5,LiteralValue 3427),(5,LiteralValue 713437532692),(0,LiteralValue 11337027696569),(5,LiteralValue 10)]),(1,LiteralValue 11781754),(1,Operator Sum (BitLength 11) [(7,LiteralValue 6)]),(5,Operator Max (SubpacketLength 3) [(7,LiteralValue 10005),(3,LiteralValue 30962),(7,LiteralValue 64960)]),(2,LiteralValue 95)])
loadInput fileName = do
  contents <- BSC.readFile $ "src/" ++ fileName
  let Right bitString = A.parseOnly parseHexAsBitstring contents
  let Right packet = A.parseOnly parsePacketBitstring $ BSC.pack bitString
  return packet

versionSum :: Packet -> Int
versionSum (version, LiteralValue _) = version
versionSum (version, Operator _ _ subpackets) = version + sum (map versionSum subpackets)

value :: Packet -> Int
value (_, LiteralValue v) = v
value (_, Operator Sum _ subpackets) = sum $ map value subpackets
value (_, Operator Product _ subpackets) = product $ map value subpackets
value (_, Operator Min _ subpackets) = minimum $ map value subpackets
value (_, Operator Max _ subpackets) = maximum $ map value subpackets
value (_, Operator GrT _ [left, right]) = if value left > value right then 1 else 0
value (_, Operator LeT _ [left, right]) = if value left < value right then 1 else 0
value (_, Operator Equ _ [left, right]) = if value left == value right then 1 else 0
value (_, Operator {}) = error "Invalid packet structure"
