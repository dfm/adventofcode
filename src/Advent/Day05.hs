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
readId = fst . head . readInt 2 (`elem` "FBLR") (fromEnum . (`elem` "BR"))