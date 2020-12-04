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
                                                ( inClass
                                                , IResult(Done)
                                                , takeByteString
                                                , Parser
                                                , choice
                                                , digit
                                                , many1
                                                , string
                                                , parse
                                                , try
                                                , feed
                                                , isDigit
                                                )
import qualified Data.ByteString.Char8         as BSC
import           Data.ByteString.Char8          ( pack
                                                , ByteString
                                                )
import           Data.Char                      ( isSpace )

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
birthYearParser = do
  string $ pack "byr:"
  year <- many1 digit
  return $ Credential BirthYear . VDate . Year $ read year

issueYearParser :: Parser Credential
issueYearParser = do
  string $ pack "iyr:"
  year <- many1 digit
  return $ Credential IssueYear . VDate . Year $ read year

-- Backtracks as 'e' is ambiguous
expirationYearParser :: Parser Credential
expirationYearParser = try $ do
  string $ pack "eyr:"
  year <- many1 digit
  return $ Credential ExpirationYear . VDate . Year $ read year

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
unitlessHeightParser = try $ do
  string $ pack "hgt:"
  height <- many1 digit
  return $ Credential Height . VHeight . UnitlessHeight $ read height

heightParser :: Parser Credential
heightParser = do
  choice [metricHeightParser, imperialHeightParser, unitlessHeightParser]

-- Backtracks as 'h' is ambiguous
hexHairColourParser :: Parser Credential
hexHairColourParser = try $ do
  string $ pack "hcl:#"
  hexCode <- takeByteString
  if validateHexCode hexCode
    then return $ Credential HairColour . VColour . HexColour $ hexCode
    else fail "Not a hex code"

-- Backtracks as 'h' is ambiguous
namedHairColourParser :: Parser Credential
namedHairColourParser = try $ do
  string $ pack "hcl:"
  Credential HairColour . VColour . NamedColour <$> takeByteString

hairColourParser :: Parser Credential
hairColourParser = do
  choice [hexHairColourParser, namedHairColourParser]

-- Backtracks as 'e' is ambiguous
hexEyeColourParser :: Parser Credential
hexEyeColourParser = try $ do
  string $ pack "ecl:#"
  hexCode <- takeByteString
  if validateHexCode hexCode
    then return $ Credential EyeColour . VColour . HexColour $ hexCode
    else fail "Not a hex code"

-- Backtracks as 'e' is ambiguous
namedEyeColourParser :: Parser Credential
namedEyeColourParser = try $ do
  string $ pack "ecl:"
  Credential EyeColour . VColour . NamedColour <$> takeByteString

eyeColourParser :: Parser Credential
eyeColourParser = do
  choice [hexEyeColourParser, namedEyeColourParser]

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
countryIDParser = do
  string $ pack "cid:"
  Credential CountryID . VID . FreeTextID <$> takeByteString

credentialParser :: Parser Credential
credentialParser = do
  choice
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
toCredential s = result
  where Done _ result = feed (parse credentialParser s) BSC.empty

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

breakOnBlankLines :: ByteString -> [ByteString]
breakOnBlankLines bs
  | BSC.null beforeBlankLine = []
  | otherwise = beforeBlankLine : breakOnBlankLines afterBlankLine
 where
  afterBlankLine          = BSC.drop (BSC.length blankLine) rest
  (beforeBlankLine, rest) = BSC.breakSubstring blankLine bs
  blankLine               = pack "\n\n"

validateHexCode :: ByteString -> Bool
validateHexCode = BSC.all (inClass "0-9a-f")

parsePeople :: ByteString -> [Credentials]
parsePeople inputString =
  map parsePersonCredentials $ breakOnBlankLines inputString

loadInput :: String -> IO [Credentials]
loadInput fileName = parsePeople <$> BSC.readFile ("src/" ++ fileName)
