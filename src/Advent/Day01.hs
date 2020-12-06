module Advent.Day01 (part1, part2) where

import qualified Data.Set as Set

part1 :: String -> Integer
part1 = sum . map readLine . lines

part2 :: String -> Integer
part2 text =
  let baseNumbers = map readLine $ lines text
      numbers = scanl (+) 0 $ cycle baseNumbers
      sets = scanl (flip Set.insert) Set.empty numbers
   in head [x | (x, s) <- zip numbers sets, Set.member x s]

readLine :: String -> Integer
readLine [] = 0
readLine (x : xs) = if x == '+' then read xs else read (x : xs)
