{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( part1,
    loadInput,
    withSwaps,
    part2,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.State qualified as ST
import Data
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bits (Bits (xor, (.|.)), (.&.))
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.List (intercalate, sort)
import Data.Map qualified as M
import Data.Set qualified as S
import Text.Printf

-- >>> A.parseOnly parseInitialValue "x00: 1"
parseInitialValue :: A.Parser (String, Int)
parseInitialValue = (,) <$> (BSC.unpack <$> A.takeWhile1 isAlphaNum <* ": ") <*> A.decimal

parseOperation :: A.Parser Operation
parseOperation = AND <$ "AND" <|> OR <$ "OR" <|> XOR <$ "XOR"

-- >>> A.parseOnly parseGate "x00 AND y00 -> z00"
-- Right (Gate {leftInput' = "x00", operation' = AND, rightInput' = "y00", output' = "z00"})
parseGate :: A.Parser Gate
parseGate =
  Gate
    <$> (BSC.unpack <$> A.takeWhile1 isAlphaNum <* A.skipSpace)
    <*> (parseOperation <* A.skipSpace)
    <*> (BSC.unpack <$> A.takeWhile1 isAlphaNum <* " -> ")
    <*> (BSC.unpack <$> A.takeWhile1 isAlphaNum)

parseInput :: A.Parser Input
parseInput = toInput <$> (parseInitialValue `A.sepBy1` A.endOfLine <* A.count 2 A.endOfLine) <*> (parseGate `A.sepBy1` A.endOfLine)
  where
    toInput initialValues gates =
      let intialValues'' = M.fromList initialValues
          gatesByOutput'' = M.fromList $ map ((,) =<< output') gates
          wires'' = S.union (M.keysSet intialValues'') $ S.fromList $ map output' gates
       in Input intialValues'' gatesByOutput'' wires''

-- >>> loadInput "example.txt"
-- Input {initialValues' = fromList [("x00",1),("x01",1),("x02",1),("y00",0),("y01",1),("y02",0)], gatesByOutput' = fromList [("z00",Gate {leftInput' = "x00", operation' = AND, rightInput' = "y00", output' = "z00"}),("z01",Gate {leftInput' = "x01", operation' = XOR, rightInput' = "y01", output' = "z01"}),("z02",Gate {leftInput' = "x02", operation' = OR, rightInput' = "y02", output' = "z02"})], wires' = fromList ["x00","x01","x02","y00","y01","y02","z00","z01","z02"]}
loadInput :: String -> IO Input
loadInput = (fromRight (Input M.empty M.empty S.empty) . A.parseOnly parseInput <$>) . BSC.readFile . ("src/" ++)

getWire :: Input -> String -> ST.State (M.Map String Int) Int
getWire input key = do
  values <- ST.get
  case M.lookup key values of
    Just value -> return value
    Nothing -> case M.lookup key (initialValues' input) of
      Just value -> do
        ST.modify $ M.insert key value
        return value
      Nothing -> case M.lookup key (gatesByOutput' input) of
        Just gate -> do
          leftValue <- getWire input (leftInput' gate)
          rightValue <- getWire input (rightInput' gate)
          let operation = case operation' gate of
                AND -> (.&.)
                OR -> (.|.)
                XOR -> xor
          let value = leftValue `operation` rightValue
          ST.modify $ M.insert (output' gate) value
          return value
        Nothing -> error $ "No value for key " ++ key

-- >>> (`getWires` ["x00", "z00"]) <$> loadInput "example.txt"
-- [1,0]
getWires :: Input -> [String] -> [Int]
getWires input wires = ST.evalState (mapM (getWire input) wires) (initialValues' input)

-- >>> (valueForLayer 'z') <$> loadInput "example.txt"
-- 4
valueForLayer :: Char -> Input -> Int
valueForLayer layer input =
  let wires = S.toAscList $ S.filter ((== layer) . head) $ wires' input
   in sum $ zipWith (\wire v -> v * (2 ^ read (tail wire))) wires $ getWires input wires

-- >>> part1 <$> loadInput "largerExample.txt"
-- 2024
part1 :: Input -> Int
part1 = valueForLayer 'z'

-- >>> (\i -> map (`bitsForLayer` i) "xyz")  <$> loadInput "input.txt"
-- [45,45,46]

bitsForLayer :: Char -> Input -> Int
bitsForLayer l = S.size . S.filter ((== l) . head) . wires'

toWire :: Char -> Int -> String
toWire l p = l : printf "%02d" p

-- >>> validate <$> loadInput "input.txt"
-- [(11,4096,2048),(12,2048,-2048),(24,33554432,16777216),(25,16777216,-16777216),(29,268435456,-268435456),(38,549755813888,274877906944)]

-- >>> validate . withSwaps <$> loadInput "input.txt"
-- []
validate :: Input -> [(Int, Int, Int)]
validate input =
  filter (\(_, _, d) -> d /= 0) $
    map (\(p, v) -> (p, v, v - (2 ^ p))) $
      [ ( p,
          valueForLayer
            'z'
            input
              { initialValues' =
                  M.union
                    (M.fromList [(toWire 'x' (p - 1), 1), (toWire 'y' (p - 1), 1)])
                    (M.map (const 0) $ initialValues' input)
              }
        )
        | p <- [1 .. bitsForLayer 'z' input - 1]
      ]

swapTwoOutputs :: String -> String -> Input -> Input
swapTwoOutputs a b input@(Input _ gates _) = input {gatesByOutput' = M.insert b (gates M.! a) {output' = b} $ M.insert a (gates M.! b) {output' = a} gates}

-- >>> A little bit of GraphViz and a whole lot of manual inspection
swapped :: [([Char], [Char])]
swapped = [("qdq", "pvb"), ("z38", "hqh"), ("z24", "mmk"), ("z11", "vkq")]

-- >>> part2
-- "hqh,mmk,pvb,qdq,vkq,z11,z24,z38"
part2 :: [Char]
part2 = intercalate "," $ sort $ concatMap (\(x, y) -> [x, y]) swapped

withSwaps :: Input -> Input
withSwaps input = foldl (\r (a, b) -> swapTwoOutputs a b r) input swapped
