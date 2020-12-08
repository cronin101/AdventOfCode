module Lib
  ( loadInput
  , hasRequiredCredentials
  , credentialsAreValid
  , readDigitsFromByteString
  ) where

import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Attoparsec.ByteString.Char8
                                                ( parseOnly
                                                , inClass
                                                , takeByteString
                                                , Parser
                                                , choice
                                                , digit
                                                , many1
                                                , string
                                                , try
                                                , isDigit
                                                )
import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( unpack
                                                , pack
                                                , ByteString
                                                )
import           Data.Char                      ( isSpace )
import           AoCUtils                       ( breakOnBlankLines
                                                , byteStringWithPrefixParser
                                                )
import           Data.Either                    ( rights )
data CredentialType = BirthYear | IssueYear | ExpirationYear | Height
    | HairColour | EyeColour | PassportID | CountryID
    deriving (Enum, Ord, Eq, Show)


data TColour = NamedColour { colourName :: ByteString } | HexColour { hexCode :: ByteString}
  deriving (Show, Eq)

newtype TDate = Year
  { year :: Int
  } deriving (Show, Eq)

data THeight =
  MetricHeight { centimeters :: Int } |
  ImperialHeight { inches :: Int } |
  UnitlessHeight { amount :: Int }
  deriving (Show, Eq)

data TID = NumericalID { code :: [Int]} | FreeTextID { _id :: ByteString }
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
  knownColours =
    S.fromList $ map pack ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

-- Creating a bunch of parsers for individual credentials...



birthYearParser :: Parser Credential
birthYearParser = byteStringWithPrefixParser
  "byr:"
  (BSC.all isDigit)
  (Credential BirthYear . VDate . Year . read . unpack)

issueYearParser :: Parser Credential
issueYearParser = byteStringWithPrefixParser
  "iyr:"
  (BSC.all isDigit)
  (Credential IssueYear . VDate . Year . read . unpack)

-- Backtracks as 'e' is ambiguous
expirationYearParser :: Parser Credential
expirationYearParser = try $ byteStringWithPrefixParser
  "eyr:"
  (BSC.all isDigit)
  (Credential ExpirationYear . VDate . Year . read . unpack)

-- Backtracks as 'h' is ambiguous
metricHeightParser :: Parser Credential
metricHeightParser = try $ do
  string $ pack "hgt:"
  centimeters <- many1 digit
  string $ pack "cm"
  return $ Credential Height . VHeight . MetricHeight $ read centimeters

-- Backtracks as 'h' is ambiguous
imperialHeightParser :: Parser Credential
imperialHeightParser = try $ do
  string $ pack "hgt:"
  inches <- many1 digit
  string $ pack "in"
  return $ Credential Height . VHeight . ImperialHeight $ read inches

-- Backtracks as 'h' is ambiguous
unitlessHeightParser :: Parser Credential
unitlessHeightParser = try $ byteStringWithPrefixParser
  "hgt:"
  (BSC.all isDigit)
  (Credential Height . VHeight . UnitlessHeight . read . unpack)

heightParser :: Parser Credential
heightParser =
  choice [metricHeightParser, imperialHeightParser, unitlessHeightParser]

-- Backtracks as 'h' is ambiguous
hexHairColourParser :: Parser Credential
hexHairColourParser = try $ byteStringWithPrefixParser
  "hcl:#"
  validateHexCode
  (Credential HairColour . VColour . HexColour)

-- Backtracks as 'h' is ambiguous
namedHairColourParser :: Parser Credential
namedHairColourParser = try $ byteStringWithPrefixParser
  "hcl:"
  (const True)
  (Credential HairColour . VColour . NamedColour)

hairColourParser :: Parser Credential
hairColourParser = choice [hexHairColourParser, namedHairColourParser]

-- Backtracks as 'e' is ambiguous
hexEyeColourParser :: Parser Credential
hexEyeColourParser = try $ byteStringWithPrefixParser
  "ecl:#"
  validateHexCode
  (Credential EyeColour . VColour . HexColour)

-- Backtracks as 'e' is ambiguous
namedEyeColourParser :: Parser Credential
namedEyeColourParser = try $ byteStringWithPrefixParser
  "ecl:"
  (const True)
  (Credential EyeColour . VColour . NamedColour)

eyeColourParser :: Parser Credential
eyeColourParser = choice [hexEyeColourParser, namedEyeColourParser]

-- Backtracks as 'p' is ambiguous
passportIDParser :: Parser Credential
passportIDParser = try $ do
  string $ pack "pid:"
  passportId <- takeByteString
  let digits = readDigitsFromByteString passportId
  return $ if length digits == BSC.length passportId
    then Credential PassportID . VID . NumericalID $ digits
    else Credential PassportID . VID . FreeTextID $ passportId

countryIDParser :: Parser Credential
countryIDParser = byteStringWithPrefixParser
  "cid:"
  (const True)
  (Credential CountryID . VID . FreeTextID)

credentialParser :: Parser Credential
credentialParser = choice
  [ birthYearParser
  , issueYearParser
  , expirationYearParser
  , heightParser
  , hairColourParser
  , eyeColourParser
  , passportIDParser
  , countryIDParser
  ]

-- All that's left is running the parser on ByteStrings

toCredential :: ByteString -> Credential
toCredential s = getCredential $ parseOnly credentialParser s
  where getCredential (Right credential) = credential

parsePersonCredentials :: ByteString -> Credentials
parsePersonCredentials singlePersonCredentials = M.fromList
  [ (t, credential)
  | credential@(Credential t _) <- map toCredential
    $ BSC.splitWith isSpace singlePersonCredentials
  ]

readDigitsFromByteString :: ByteString -> [Int]
readDigitsFromByteString bs = case BSC.uncons bs of
  Nothing           -> []
  Just (char, rest) -> if isDigit char
    then read [char] : readDigitsFromByteString rest
    else readDigitsFromByteString rest

validateHexCode :: ByteString -> Bool
validateHexCode = BSC.all (inClass "0-9a-f")

parsePeople :: ByteString -> [Credentials]
parsePeople inputString =
  map parsePersonCredentials $ breakOnBlankLines inputString

loadInput :: String -> IO [Credentials]
loadInput fileName = parsePeople <$> BSC.readFile ("src/" ++ fileName)
