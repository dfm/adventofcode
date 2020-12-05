module Advent.Day05 where

import Data.Sort (sort)
import Numeric (readInt)

part1 :: String -> Int
part1 text = maximum $ map readId (lines text)

part2 :: String -> Int
part2 text =
  let ids = sort $ map readId (lines text)
      diffs = filter (\(x, y) -> y - x == 2) $ zip ids (drop 1 ids)
   in 1 + fst (head diffs)

readId :: String -> Int
readId text =
  let (row, more) = readBinary 'F' 'B' text
      (col, _) = readBinary 'L' 'R' more
   in row * 8 + col

readBinary :: (Integral a) => Char -> Char -> String -> (a, String)
readBinary zero one =
  let getValue x = if x == zero then 0 else 1
   in head . readInt 2 (`elem` [zero, one]) getValue