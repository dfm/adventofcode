module Advent.Day04 where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

part1 :: String -> Int
part1 text = length $ filter isValidPassport (parsePassports text)

part2 :: String -> Int
part2 text = length $ filter ((&&) <$> allValidFields <*> isValidPassport) (parsePassports text)

isValidPassport :: [Field] -> Bool
isValidPassport passport = requiredFields `Set.isSubsetOf` Set.fromList (map fName passport)

allValidFields :: [Field] -> Bool
allValidFields passport =
  let checkFieldValid (Field CountryID _) = True
      checkFieldValid (Field _ Nothing) = False
      checkFieldValid (Field _ (Just _)) = True
   in all checkFieldValid passport

-- Parsing crap: types
type Parser = Parsec Void String

data Field = Field {fName :: FieldName, fContents :: Maybe FieldValue} deriving (Eq, Show)

data FieldName
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving (Eq, Show, Ord)

newtype FieldValue = FieldValue String deriving (Eq, Show, Ord)

-- Parsing crap: token parsers
pFieldName :: Parser FieldName
pFieldName =
  choice
    [ BirthYear <$ string "byr",
      IssueYear <$ string "iyr",
      ExpirationYear <$ string "eyr",
      Height <$ string "hgt",
      HairColor <$ string "hcl",
      EyeColor <$ string "ecl",
      PassportID <$ string "pid",
      CountryID <$ string "cid"
    ]

pYearRange :: Int -> Int -> Parser (Maybe FieldValue)
pYearRange mn mx = do
  year <- some digitChar
  let nYear = read year :: Int
  if (mn <= nYear) && (nYear <= mx)
    then return $ Just (FieldValue year)
    else return Nothing

-- Parsing crap: field validation
pFieldValue :: FieldName -> Parser (Maybe FieldValue)
pFieldValue BirthYear = pYearRange 1920 2002
pFieldValue IssueYear = pYearRange 2010 2020
pFieldValue ExpirationYear = pYearRange 2020 2030
pFieldValue Height = do
  height <- some digitChar
  units <- string "in" <|> string "cm"
  let nHeight = read height :: Int
      isValid = if units == "in" then 59 <= nHeight && nHeight <= 76 else 150 <= nHeight && nHeight <= 193
  if isValid
    then return $ Just (FieldValue height)
    else return Nothing
pFieldValue HairColor = do
  _ <- char '#'
  color <- some hexDigitChar
  if length color == 6
    then return $ Just (FieldValue color)
    else return Nothing
pFieldValue EyeColor = do
  color <-
    string "amb"
      <|> string "blu"
      <|> string "brn"
      <|> string "gry"
      <|> string "grn"
      <|> string "hzl"
      <|> string "oth"
  return $ Just (FieldValue color)
pFieldValue PassportID = do
  number <- some digitChar
  if length number == 9
    then return $ Just (FieldValue number)
    else return Nothing
pFieldValue _ = do
  c <- some (alphaNumChar <|> char '#')
  return $ Just (FieldValue c)

pField :: Parser Field
pField = do
  n <- pFieldName
  _ <- char ':'
  value <-
    optional . try $
      ( do
          field <- pFieldValue n
          _ <- eof
          return field
      )
  case value of
    Nothing -> return $ Field n Nothing
    Just x -> return $ Field n x

-- Parsing crap: application functions
parseToken :: String -> Field
parseToken text = case parse pField "" text of
  Left bundle -> error (errorBundlePretty bundle)
  Right field -> field

parsePassports :: String -> [[Field]]
parsePassports text = map (map parseToken . words) $ splitOn "\n\n" text

requiredFields :: Set.Set FieldName
requiredFields =
  Set.fromList
    [ BirthYear,
      IssueYear,
      ExpirationYear,
      Height,
      HairColor,
      EyeColor,
      PassportID
    ]
