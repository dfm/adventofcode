module Advent.Day05 (part1, part2) where

import Data.Sort (sort)
import Numeric (readInt)

part1 :: String -> Int
part1 = maximum . map readId . lines

part2 :: String -> Int
part2 = (+ 1) . head . findGaps . sort . map readId . lines

readId :: String -> Int
readId = fst . head . readInt 2 (`elem` "FBLR") (fromEnum . (`elem` "BR"))

findGaps :: [Int] -> [Int]
findGaps (x : y : zs) = if y - x == 2 then x : findGaps (y : zs) else findGaps (y : zs)
findGaps _ = []