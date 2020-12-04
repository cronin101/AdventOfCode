module Lib
  ( loadInput
  , hasRequiredCredentials
  , credentialsAreValid
  ) where

import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Text.Parsec.ByteString         ( Parser )
import           Text.Parsec                    ( hexDigit
                                                , anyChar
                                                , alphaNum
                                                , (<|>)
                                                , digit
                                                , many1
                                                , string
                                                , parse
                                                , letter
                                                , try
                                                )
import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( pack
                                                , ByteString
                                                )
import           Data.Char                      ( isSpace )

data CredentialType = BirthYear | IssueYear | ExpirationYear | Height
    | HairColour | EyeColour | PassportID | CountryID
    deriving (Enum, Ord, Eq, Show)


data TColour = NamedColour { colourName :: String } | HexColour { hexCode :: String}
  deriving (Show, Eq)

newtype TDate = Year
  { year :: Int
  } deriving (Show, Eq)

data THeight =
  MetricHeight { centimeters :: Int } |
  ImperialHeight { inches :: Int } |
  UnitlessHeight { amount :: Int }
  deriving (Show, Eq)

data TID = NumericalID { code :: [Int]} | FreeTextID { _id :: String }
  deriving (Show, Eq)

data CredentialValue =
    VDate TDate |
    VHeight THeight|
    VColour TColour |
    VID TID
  deriving (Show, Eq)

data Credential = Credential CredentialType CredentialValue
  deriving (Show, Eq)

type Credentials = Map CredentialType Credential

toBirthYear :: Int -> Credential
toBirthYear = Credential BirthYear . VDate . Year
toIssueYear :: Int -> Credential
toIssueYear = Credential IssueYear . VDate . Year
toExpirationYear :: Int -> Credential
toExpirationYear = Credential ExpirationYear . VDate . Year
toMetricHeight :: Int -> Credential
toMetricHeight = Credential Height . VHeight . MetricHeight
toImperialHeight :: Int -> Credential
toImperialHeight = Credential Height . VHeight . ImperialHeight
toUnitlessHeight :: Int -> Credential
toUnitlessHeight = Credential Height . VHeight . UnitlessHeight
toHexHairColour :: String -> Credential
toHexHairColour = Credential HairColour . VColour . HexColour
toNamedHairColour :: String -> Credential
toNamedHairColour = Credential HairColour . VColour . NamedColour
toNamedEyeColour :: String -> Credential
toNamedEyeColour = Credential EyeColour . VColour . NamedColour
toHexEyeColour :: String -> Credential
toHexEyeColour = Credential EyeColour . VColour . HexColour
toFreeTextPassportId :: String -> Credential
toFreeTextPassportId = Credential PassportID . VID . FreeTextID
toNumericalPassPortId :: [Int] -> Credential
toNumericalPassPortId = Credential PassportID . VID . NumericalID
toCountryId :: String -> Credential
toCountryId = Credential CountryID . VID . FreeTextID

-- After all the type system boilerplate, the real fun can begin
-- Validation Criteria....

allCredentialTypes :: [CredentialType]
allCredentialTypes = enumFrom $ toEnum 0

-- Check if all credentials except countryId are present
hasRequiredCredentials :: Map CredentialType Credential -> Bool
hasRequiredCredentials credentials =
  (length allCredentialTypes - 1) == M.size (M.delete CountryID credentials)

credentialsAreValid :: Credentials -> Bool
credentialsAreValid = all (credentialIsValid . snd) . M.toList

-- The king of pattern matching...
credentialIsValid :: Credential -> Bool
credentialIsValid credential = case credential of
  (Credential BirthYear (VDate (Year year))) -> year >= 1920 && year <= 2002
  (Credential IssueYear (VDate (Year year))) -> year >= 2010 && year <= 2020
  (Credential ExpirationYear (VDate (Year year))) ->
    year >= 2020 && year <= 2030
  (Credential Height (VHeight (MetricHeight centimeters))) ->
    centimeters >= 150 && centimeters <= 193
  (Credential Height (VHeight (ImperialHeight inches))) ->
    inches >= 59 && inches <= 76
  (Credential Height     _                      ) -> False
  (Credential HairColour (VColour (HexColour _))) -> True
  (Credential HairColour _                      ) -> False
  (Credential EyeColour (VColour (NamedColour namedColour))) ->
    S.member namedColour knownColours
  (Credential EyeColour  _                         ) -> False
  (Credential PassportID (VID (NumericalID digits))) -> length digits == 9
  (Credential PassportID _                         ) -> False
  (Credential CountryID  _                         ) -> True
 where
  knownColours = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

-- Creating a bunch of parsers for individual credentials...

birthYearParser :: Parser Credential
birthYearParser = do
  string "byr:"
  year <- many1 digit
  return $ toBirthYear $ read year

issueYearParser :: Parser Credential
issueYearParser = do
  string "iyr:"
  year <- many1 digit
  return $ toIssueYear $ read year

-- Backtracks as 'e' is ambiguous
expirationYearParser :: Parser Credential
expirationYearParser = try $ do
  string "eyr:"
  year <- many1 digit
  return $ toExpirationYear $ read year

-- Backtracks as 'h' is ambiguous
metricHeightParser :: Parser Credential
metricHeightParser = try $ do
  string "hgt:"
  centimeters <- many1 digit
  string "cm"
  return $ toMetricHeight $ read centimeters

-- Backtracks as 'h' is ambiguous
imperialHeightParser :: Parser Credential
imperialHeightParser = try $ do
  string "hgt:"
  inches <- many1 digit
  string "in"
  return $ toImperialHeight $ read inches

-- Backtracks as 'h' is ambiguous
unitlessHeightParser :: Parser Credential
unitlessHeightParser = try $ do
  string "hgt:"
  height <- many1 digit
  return $ toUnitlessHeight $ read height

heightParser :: Parser Credential
heightParser = do
  metricHeightParser <|> imperialHeightParser <|> unitlessHeightParser

-- Backtracks as 'h' is ambiguous
hexHairColourParser :: Parser Credential
hexHairColourParser = try $ do
  string "hcl:#"
  hexCode <- many1 hexDigit
  return $ toHexHairColour hexCode

-- Backtracks as 'h' is ambiguous
namedHairColourParser :: Parser Credential
namedHairColourParser = try $ do
  string "hcl:"
  colour <- many1 alphaNum
  return $ toNamedHairColour colour

hairColourParser :: Parser Credential
hairColourParser = do
  hexHairColourParser <|> namedHairColourParser

-- Backtracks as 'e' is ambiguous
hexEyeColourParser :: Parser Credential
hexEyeColourParser = try $ do
  string "ecl:#"
  hexCode <- many1 hexDigit
  return $ toHexEyeColour hexCode

-- Backtracks as 'e' is ambiguous
namedEyeColourParser :: Parser Credential
namedEyeColourParser = try $ do
  string "ecl:"
  colour <- many1 letter
  return $ toNamedEyeColour colour

eyeColourParser :: Parser Credential
eyeColourParser = do
  hexEyeColourParser <|> namedEyeColourParser

-- Backtracks as 'p' is ambiguous
numericalPassportIDParser :: Parser Credential
numericalPassportIDParser = try $ do
  string "pid:"
  digits <- many1 digit
  return $ toNumericalPassPortId $ map (read . (: [])) digits

-- Backtracks as 'p' is ambiguous
freeTextPassportIDParser :: Parser Credential
freeTextPassportIDParser = try $ do
  string "pid:"
  passportId <- many1 anyChar
  return $ toFreeTextPassportId passportId

passportIDParser :: Parser Credential
passportIDParser = do
  numericalPassportIDParser <|> freeTextPassportIDParser

countryIDParser :: Parser Credential
countryIDParser = do
  string "cid:"
  countryId <- many1 anyChar
  return $ toCountryId countryId

credentialParser :: Parser Credential
credentialParser = do
  birthYearParser
    <|> issueYearParser
    <|> expirationYearParser
    <|> heightParser
    <|> hairColourParser
    <|> eyeColourParser
    <|> passportIDParser
    <|> countryIDParser

-- All that's left is running the parser on ByteStrings

toCredential :: ByteString -> Credential
toCredential s = result where Right result = parse credentialParser "" s

parsePersonCredentials :: ByteString -> Credentials
parsePersonCredentials singlePersonCredentials = M.fromList
  [ (t, credential)
  | credential@(Credential t _) <- map toCredential
    $ BSC.splitWith isSpace singlePersonCredentials
  ]

breakOnBlankLines :: ByteString -> [ByteString]
breakOnBlankLines bs
  | BSC.null beforeBlankLine = []
  | otherwise = beforeBlankLine : breakOnBlankLines afterBlankLine
 where
  afterBlankLine          = BSC.drop (BSC.length blankLine) rest
  (beforeBlankLine, rest) = BSC.breakSubstring blankLine bs
  blankLine               = pack "\n\n"

parsePeople :: ByteString -> [Credentials]
parsePeople inputString =
  map parsePersonCredentials $ breakOnBlankLines inputString

loadInput :: String -> IO [Credentials]
loadInput fileName = parsePeople <$> BSC.readFile ("src/" ++ fileName)
