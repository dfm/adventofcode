module Advent.Day02 where

import Text.Parsec
  ( ParseError,
    Parsec,
    anyChar,
    char,
    digit,
    many1,
    parse,
  )

data PasswordRule = PasswordRule {minCount :: Int, maxCount :: Int, character :: Char, password :: String} deriving (Show)

part1 :: [String] -> Either ParseError Integer
part1 text =
  let rules = map (parse parseRule "") text
      checks = map (checkPasswordRule <$>) rules
   in foldl mySum (Right 0) checks

part2 :: [String] -> Either ParseError Integer
part2 text =
  let rules = map (parse parseRule "") text
      checks = map (checkPasswordRule2 <$>) rules
   in foldl mySum (Right 0) checks

parseRule :: Parsec String st PasswordRule
parseRule = do
  minNum <- many1 digit
  _ <- char '-'
  maxNum <- many1 digit
  _ <- char ' '
  token <- anyChar
  _ <- char ':'
  _ <- char ' '
  PasswordRule (read minNum) (read maxNum) token <$> many1 anyChar

checkPasswordRule :: PasswordRule -> Integer
checkPasswordRule rule =
  let target = character rule
      word = password rule
      count = foldl (\acc c -> if c == target then acc + 1 else acc) 0 word
   in if count >= minCount rule && count <= maxCount rule then 1 else 0

checkPasswordRule2 :: PasswordRule -> Integer
checkPasswordRule2 rule =
  let target = character rule
      word = password rule
      at1 = word !! (minCount rule - 1)
      at2 = word !! (maxCount rule - 1)
   in if (at1 == target) /= (at2 == target) then 1 else 0

mySum :: Either ParseError Integer -> Either ParseError Integer -> Either ParseError Integer
mySum (Right acc) (Right x) = Right $ acc + x
mySum _ (Left x) = Left x
mySum (Left x) _ = Left x
