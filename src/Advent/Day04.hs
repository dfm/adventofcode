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
allValidFields = all isValidField

-- Parsing crap: types
type Parser = Parsec Void String

data Field = Field {fName :: FieldName, fContents :: String} deriving (Eq, Show)

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

-- Parsing crap: application functions
parseToken :: String -> Field
parseToken = handleParsingError pField

parsePassports :: String -> [[Field]]
parsePassports text = map (map parseToken . words) $ splitOn "\n\n" text

-- Parsing crap: token parsers
handleParsingError :: Parser p -> String -> p
handleParsingError p text = case parse p "" text of
  Left bundle -> error (errorBundlePretty bundle)
  Right x -> x

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

pField :: Parser Field
pField = do
  n <- pFieldName
  _ <- char ':'
  v <- many asciiChar
  return $ Field n v

pHeight :: Parser (Int, String)
pHeight = do
  height <- many digitChar
  unit <- string "in" <|> string "cm"
  return (read height, unit)

pColor :: Parser String
pColor = do
  _ <- char '#'
  many hexDigitChar

-- Parsing crap: field validation
isInRange :: Int -> Int -> Int -> Bool
isInRange mn mx = (&&) <$> (>= mn) <*> (<= mx)

isValidField :: Field -> Bool
isValidField (Field BirthYear x) = isInRange 1920 2002 $ read x
isValidField (Field IssueYear x) = isInRange 2010 2020 $ read x
isValidField (Field ExpirationYear x) = isInRange 2020 2030 $ read x
isValidField (Field Height x) = case parse pHeight "" x of
  Left _ -> False
  Right (height, unit) ->
    let (mn, mx) = if unit == "in" then (59, 76) else (150, 193)
     in isInRange mn mx height
isValidField (Field HairColor x) = case parse pColor "" x of
  Left _ -> False
  Right color -> length color == 6
isValidField (Field EyeColor x) = Set.member x validEyeColors
isValidField (Field PassportID x) = length x == 9 && all (`Set.member` Set.fromList "0123456789") x
isValidField _ = True

-- Validation sets
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

validEyeColors :: Set.Set String
validEyeColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
