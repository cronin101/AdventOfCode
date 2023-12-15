{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Char (ord)
import Data.Either (fromRight)
import Data.Map qualified as M

data Lens = Lens {lensName :: String, lensFocalLength :: Int} deriving (Show, Eq, Ord)

data Operation = Add {lens :: Lens} | Remove {removedName :: String} deriving (Show)

data Instruction = Instruction {op :: Operation, instructionHash :: Int} deriving (Show)

type Boxes = M.Map Int (M.Map String (Int, Lens))

-- >>> hash "rn=1"
-- 30
hash :: String -> Int
hash = foldl (\r c -> ((r + ord c) * 17) `mod` 256) 0

-- >>> A.parseOnly parseAddLens "rn=1"
-- Right (Instruction {op = Add {lens = Lens {lensName = "rn", lensFocalLength = 1}}, instructionHash = 30})
parseAddLens :: A.Parser Instruction
parseAddLens = do
  name <- BSC.unpack <$> A.takeWhile1 A.isAlpha_ascii <* "="
  focalLength <- A.decimal
  return $ Instruction (Add (Lens name focalLength)) (hash $ name ++ "=" ++ show focalLength)

-- >>> A.parseOnly parseRemoveLens "cm-"
-- Right (Instruction {op = Remove {removedName = "cm"}, instructionHash = 253})
parseRemoveLens :: A.Parser Instruction
parseRemoveLens = do
  name <- BSC.unpack <$> A.takeWhile1 A.isAlpha_ascii <* "-"
  return $ Instruction (Remove name) (hash $ name ++ "-")

parseInstruction :: A.Parser Instruction
parseInstruction = parseAddLens <|> parseRemoveLens

-- >>> A.parseOnly parseInstructions "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
-- Right [Instruction {op = Add {lens = Lens {lensName = "rn", lensFocalLength = 1}}, instructionHash = 30},Instruction {op = Remove {removedName = "cm"}, instructionHash = 253},Instruction {op = Add {lens = Lens {lensName = "qp", lensFocalLength = 3}}, instructionHash = 97},Instruction {op = Add {lens = Lens {lensName = "cm", lensFocalLength = 2}}, instructionHash = 47},Instruction {op = Remove {removedName = "qp"}, instructionHash = 14},Instruction {op = Add {lens = Lens {lensName = "pc", lensFocalLength = 4}}, instructionHash = 180},Instruction {op = Add {lens = Lens {lensName = "ot", lensFocalLength = 9}}, instructionHash = 9},Instruction {op = Add {lens = Lens {lensName = "ab", lensFocalLength = 5}}, instructionHash = 197},Instruction {op = Remove {removedName = "pc"}, instructionHash = 48},Instruction {op = Add {lens = Lens {lensName = "pc", lensFocalLength = 6}}, instructionHash = 214},Instruction {op = Add {lens = Lens {lensName = "ot", lensFocalLength = 7}}, instructionHash = 231}]
parseInstructions :: A.Parser [Instruction]
parseInstructions = parseInstruction `A.sepBy1` ","

-- >>> loadInput "example.txt"
-- [Instruction {op = Add {lens = Lens {lensName = "rn", lensFocalLength = 1}}, instructionHash = 30},Instruction {op = Remove {removedName = "cm"}, instructionHash = 253},Instruction {op = Add {lens = Lens {lensName = "qp", lensFocalLength = 3}}, instructionHash = 97},Instruction {op = Add {lens = Lens {lensName = "cm", lensFocalLength = 2}}, instructionHash = 47},Instruction {op = Remove {removedName = "qp"}, instructionHash = 14},Instruction {op = Add {lens = Lens {lensName = "pc", lensFocalLength = 4}}, instructionHash = 180},Instruction {op = Add {lens = Lens {lensName = "ot", lensFocalLength = 9}}, instructionHash = 9},Instruction {op = Add {lens = Lens {lensName = "ab", lensFocalLength = 5}}, instructionHash = 197},Instruction {op = Remove {removedName = "pc"}, instructionHash = 48},Instruction {op = Add {lens = Lens {lensName = "pc", lensFocalLength = 6}}, instructionHash = 214},Instruction {op = Add {lens = Lens {lensName = "ot", lensFocalLength = 7}}, instructionHash = 231}]
loadInput :: [Char] -> IO [Instruction]
loadInput = (fromRight [] . A.parseOnly parseInstructions <$>) . BSC.readFile . ("src/" ++)

-- >>> part1 <$> loadInput "example.txt"
-- 1320
part1 :: [Instruction] -> Int
part1 = sum . map instructionHash

-- >>> followInstructions <$> loadInput "example.txt"
-- fromList [(0,fromList [("cm",(1,Lens {lensName = "cm", lensFocalLength = 2})),("rn",(0,Lens {lensName = "rn", lensFocalLength = 1}))]),(1,fromList []),(3,fromList [("ab",(1,Lens {lensName = "ab", lensFocalLength = 5})),("ot",(0,Lens {lensName = "ot", lensFocalLength = 7})),("pc",(2,Lens {lensName = "pc", lensFocalLength = 6}))])]
followInstructions :: [Instruction] -> Boxes
followInstructions = foldl follow M.empty
  where
    alterBox bHash bs fn = M.insert bHash (fn (M.findWithDefault M.empty bHash bs)) bs
    follow bs (Instruction (Add l@(Lens name _)) _) = alterBox (hash name) bs $
      \b ->
        case M.lookup name b of
          Nothing -> M.insert name (1 + maximum ((-1 :) $ map fst $ M.elems b), l) b
          Just (idx, _) -> M.insert name (idx, l) b
    follow bs (Instruction (Remove name) _) = alterBox (hash name) bs $
      \b -> case M.lookup name b of
        Nothing -> b
        Just (idx, _) -> M.map (\(idx', l) -> (if idx' > idx then idx' - 1 else idx', l)) $ M.delete name b

-- >>> focusingPowers . followInstructions <$> loadInput "example.txt"
-- [4,1,40,28,72]
focusingPowers :: Boxes -> [Int]
focusingPowers bs =
  concatMap
    ( \(boxN, box) ->
        map (\(_, (slot, l)) -> product [boxN + 1, slot + 1, lensFocalLength l]) $
          M.toList box
    )
    $ M.toList bs

-- >>> part2 <$> loadInput "example.txt"
-- 145
part2 :: [Instruction] -> Int
part2 = sum . focusingPowers . followInstructions
