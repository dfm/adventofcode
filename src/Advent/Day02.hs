module Advent.Day02 where

import Data.List.Split (splitOn)

data PasswordRule = PasswordRule {minCount :: Int, maxCount :: Int, character :: Char, password :: String} deriving (Show)

part1 :: [String] -> Int
part1 text =
  let rules = map parseRule text
      checks = filter checkPasswordRule rules
   in length checks

part2 :: [String] -> Int
part2 text =
  let rules = map parseRule text
      checks = filter checkPasswordRule2 rules
   in length checks

parseRule :: String -> PasswordRule
parseRule text =
  let [inds, c : _, word] = words text
      [idx1, idx2] = splitOn "-" inds
   in PasswordRule (read idx1) (read idx2) c word

checkPasswordRule :: PasswordRule -> Bool
checkPasswordRule rule =
  let target = character rule
      word = password rule
      count = length $ filter (== target) word
   in count >= minCount rule && count <= maxCount rule

checkPasswordRule2 :: PasswordRule -> Bool
checkPasswordRule2 rule =
  let target = character rule
      word = password rule
      at1 = word !! (minCount rule - 1)
      at2 = word !! (maxCount rule - 1)
   in (at1 == target) /= (at2 == target)
