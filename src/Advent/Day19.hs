module Advent.Day19 (part1, part2) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Regex.TDFA ((=~))

part1 :: Bool -> String -> Int
part1 _ text =
  let (rules, messages) = readInput text
      re = '^' : getRegex rules "0" ++ "$"
      doMatch :: String -> String
      doMatch x = x =~ re
   in length $ filter ((/= "") . doMatch) messages

part2 :: Bool -> String -> Int
part2 _ text =
  let (rules, messages) = readInput text
      prefix = '^' : getRegex rules "42"
      suffix = getRegex rules "31" ++ "$"
   in length $ filter (checkMessage prefix suffix) messages

type RuleSet = Map String String

readInput :: String -> (RuleSet, [String])
readInput text =
  let [rules, messages] = splitOn "\n\n" text
   in (foldl readRule M.empty (lines rules), lines messages)

readRule :: RuleSet -> String -> RuleSet
readRule cache text =
  let (name, _, body) = text =~ ": " :: (String, String, String)
   in M.insert name body cache

getRegex :: RuleSet -> String -> String
getRegex _ ['"', c, '"'] = [c]
getRegex rules "8" =
  let a = getRegex rules (rules M.! "42")
   in '(' : a ++ ")+"
getRegex rules "11" =
  let a = getRegex rules (rules M.! "42")
      b = getRegex rules (rules M.! "31")
      set = map (\n -> concat (["("] ++ replicate n a ++ replicate n b ++ [")"])) [1 .. 5]
   in '(' : intercalate "|" set ++ ")"
getRegex rules rule =
  let options = splitOn " | " rule
      res = map (concatMap (getRegex rules . (rules M.!)) . words) options
   in "(" ++ intercalate "|" res ++ ")"

matchPrefix :: String -> (Int, String) -> (Int, String)
matchPrefix prefix (count, text) =
  let (_, match, rest) = text =~ prefix :: (String, String, String)
   in if null match then (count, text) else matchPrefix prefix (count + 1, rest)

matchSuffix :: String -> (Int, String) -> (Int, String)
matchSuffix suffix (count, text) =
  let (rest, match, _) = text =~ suffix :: (String, String, String)
   in if null match then (count, text) else matchSuffix suffix (count + 1, rest)

checkMessage :: String -> String -> String -> Bool
checkMessage prefix suffix message =
  let (c1, rest) = matchPrefix prefix (0, message)
      (c2, left) = matchSuffix suffix (0, rest)
   in c1 > c2 && c2 >= 1 && null left