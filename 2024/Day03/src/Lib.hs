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
import Data.Either (fromRight)
import Data.Maybe (catMaybes)

-- The corrupted memory contains instructions to multiply numbers.
-- This parser extracts a single valid multiplication instruction.
-- >>> A.parseOnly parseMult "mul(44,46)"
-- Right (44,46)
parseMult :: A.Parser (Int, Int)
parseMult = (,) <$> ("mul(" *> A.decimal <* ",") <*> A.decimal <* ")"

-- The corrupted memory contains many invalid characters.
-- This parser extracts all valid multiplication instructions.
-- >>> A.parseOnly parseMults "%mul(914,398)\n+mul(720,985)how(340,568)"
-- Right [(914,398),(720,985)]
parseMults :: A.Parser [(Int, Int)]
parseMults = catMaybes <$> A.many1 (Just <$> parseMult <|> Nothing <$ A.anyChar)

-- The corrupted memory contains conditional statements that enable or disable multiplications.
-- This parser extracts only the enabled multiplication instructions.
-- >>> A.parseOnly parseEnabledMults "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- Right [(2,4),(8,5)]
parseEnabledMults :: A.Parser [(Int, Int)]
parseEnabledMults = concat <$> A.many1 ((parseMultsUntil "don't()" <* skipUntil "do()") <|> parseMults)
  where
    parseInnerMults = fromRight [] . A.parseOnly parseMults . BSC.pack
    parseMultsUntil p = parseInnerMults <$> A.manyTill A.anyChar p
    skipUntil p = A.manyTill A.anyChar (p <|> "" <$ A.endOfInput)

-- Part 1: Sum the results of all valid multiplication instructions in the corrupted memory.
-- >>> part1 <$> loadInput "example1.txt"
-- 161
part1 :: BSC.ByteString -> Int
part1 = sum . map (uncurry (*)) . getMults
  where
    getMults = fromRight [] . A.parseOnly parseMults

-- Part 2: Sum the results of only the enabled multiplication instructions in the corrupted memory.
-- >>> part2 <$> loadInput "example2.txt"
-- 48
part2 :: BSC.ByteString -> Int
part2 = sum . map (uncurry (*)) . onlyEnabled
  where
    onlyEnabled = fromRight [] . A.parseOnly parseEnabledMults

-- Load the corrupted memory from a file.
-- >>> loadInput "example1.txt"
-- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
loadInput :: [Char] -> IO BSC.ByteString
loadInput = BSC.readFile . ("src/" ++)
