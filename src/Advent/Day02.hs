module Advent.Day02 where

import Data.List.Split (splitOn)

data PasswordRule = PasswordRule Int Int Char String deriving (Show)

part1 :: [String] -> Int
part1 = part checkPasswordRule

part2 :: [String] -> Int
part2 = part checkPasswordRule2

part :: (PasswordRule -> Bool) -> [String] -> Int
part checker text = length $ filter (checker . parseRule) text

parseRule :: String -> PasswordRule
parseRule text =
  let [inds, c : _, word] = words text
      [idx1, idx2] = splitOn "-" inds
   in PasswordRule (read idx1) (read idx2) c word

checkPasswordRule :: PasswordRule -> Bool
checkPasswordRule (PasswordRule mn mx target word) =
  let count = length $ filter (== target) word
   in count >= mn && count <= mx

checkPasswordRule2 :: PasswordRule -> Bool
checkPasswordRule2 (PasswordRule idx1 idx2 target word) =
  let at1 = word !! (idx1 - 1)
      at2 = word !! (idx2 - 1)
   in (at1 == target) /= (at2 == target)
