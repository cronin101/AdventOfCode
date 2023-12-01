{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
  )
where

import Data.Attoparsec.ByteString.Char8 (letter_ascii)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Attoparsec.Combinator qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Maybe (catMaybes)

-- >>> A.parse (stringWithRewindLast "eight") "eightwo"
-- Done "two" "eight"
stringWithRewindLast :: BSC.ByteString -> A.Parser BSC.ByteString
stringWithRewindLast bs = A.string (BSC.init bs) <* A.lookAhead (A.char $ BSC.last bs) $> bs

spelledDigits :: [A.Parser Char]
spelledDigits =
  zipWith
    (\str val -> stringWithRewindLast str $> val)
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    ['1' .. '9']

digitParser :: Bool -> A.Parser Char
digitParser acceptSpelledDigits = A.choice (A.digit : [spelledDigit | acceptSpelledDigits, spelledDigit <- spelledDigits])

-- Progresses through the string to find matching digits, throwing away a character for each failure
-- >>> A.parseOnly (parseFirstAndLastDigit True) "eightwone"
-- Right 81
parseFirstAndLastDigit :: Bool -> A.Parser Int
parseFirstAndLastDigit acceptSpelledDigits = do
  numbers <- catMaybes <$> A.many1 (A.choice [Just <$> digitParser acceptSpelledDigits, letter_ascii $> Nothing])
  let twoDigitNumber = [head numbers, last numbers]
  return $ read twoDigitNumber

parseInput :: Bool -> A.Parser [Int]
parseInput acceptSpelledDigits = A.sepBy1 (parseFirstAndLastDigit acceptSpelledDigits) A.endOfLine

-- >>> loadInput False "example.txt"
-- [12,38,15,77]
-- >>> loadInput True "example2.txt"
-- [29,82,23,34,42,24,77]
loadInput :: Bool -> [Char] -> IO [Int]
loadInput acceptSpelledDigits = (fromRight [] . A.parseOnly (parseInput acceptSpelledDigits) <$>) . BSC.readFile . ("src/" ++)
