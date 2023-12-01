{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInput,
  )
where

import Data.Attoparsec.ByteString.Char8 (letter_ascii)
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Maybe (catMaybes)

-- A hacky parser that will Parse () if there is a match but leave the character in the stream
-- >>> A.parseOnly (parseIfPeekMatches 'a') "abc"
-- Right ()
parseIfPeekMatches :: Char -> A.Parser ()
parseIfPeekMatches match = do
  char <- A.peekChar
  case char of
    Just c -> if c == match then return () else fail "Not a match"
    Nothing -> fail "End of input"

-- Used to match eightwone, etc. Leaves the first digit of the next number in the stream
-- >>> A.parse (stringWithPeekLast "eight") "eightwone"
-- Done "twone" "eight"
stringWithPeekLast :: BSC.ByteString -> A.Parser BSC.ByteString
stringWithPeekLast bs = A.string (BSC.init bs) *> parseIfPeekMatches (BSC.last bs) $> bs

-- Does what it says on the tin
-- >>> A.parseOnly (digitParser True) "1"
-- Right '1'
-- >>> A.parseOnly (digitParser False) "one"
-- Left "Failed reading: empty"
-- >>> A.parseOnly (digitParser True) "one"
-- Right '1'
digitParser :: Bool -> A.Parser Char
digitParser acceptSpelledDigits =
  A.choice
    ( A.digit
        : ( if acceptSpelledDigits
              then
                [ stringWithPeekLast "one" $> '1',
                  stringWithPeekLast "two" $> '2',
                  stringWithPeekLast "three" $> '3',
                  stringWithPeekLast "four" $> '4',
                  stringWithPeekLast "five" $> '5',
                  stringWithPeekLast "six" $> '6',
                  stringWithPeekLast "seven" $> '7',
                  stringWithPeekLast "eight" $> '8',
                  stringWithPeekLast "nine" $> '9'
                ]
              else []
          )
    )

-- Progresses through the string to find matching digits, throwing away a character for each failure
-- >>> A.parseOnly (parseFirstAndLastDigit True) "eightwone"
-- Right 81
parseFirstAndLastDigit :: Bool -> A.Parser Int
parseFirstAndLastDigit acceptSpelledDigits = do
  numbers <-
    catMaybes
      <$> A.many1
        ( A.choice
            [ Just <$> digitParser acceptSpelledDigits,
              letter_ascii $> Nothing
            ]
        )
  let twoDigitNumber = [head numbers, last numbers]
  return $ read twoDigitNumber

-- >>> loadInput False "example.txt"
-- [12,38,15,77]
-- >>> loadInput True "example2.txt"
-- [29,82,23,34,42,24,77]
loadInput :: Bool -> [Char] -> IO [Int]
loadInput acceptSpelledDigits = (map (fromRight 0 . A.parseOnly (parseFirstAndLastDigit acceptSpelledDigits)) . BSC.lines <$>) . BSC.readFile . ("src/" ++)
