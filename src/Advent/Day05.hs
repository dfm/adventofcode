module Advent.Day05 (part1, part2) where

import Data.Sort (sort)
import Numeric (readInt)

part1 :: Bool -> String -> Int
part1 _ = maximum . map readId . lines

part2 :: Bool -> String -> Int
part2 _ = (+ 1) . head . findGaps . sort . map readId . lines

readId :: String -> Int
readId = fst . head . readInt 2 (`elem` "FBLR") (fromEnum . (`elem` "BR"))

findGaps :: [Int] -> [Int]
findGaps (x : y : zs)
  | y - x == 2 = x : findGaps (y : zs)
  | otherwise = findGaps (y : zs)
findGaps _ = []