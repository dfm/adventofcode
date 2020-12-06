module Advent.Day02 (part1, part2) where

import Data.List (group, sort)
import qualified Data.Set as Set

part1 :: String -> Int
part1 text =
  let countNums = Set.fromList . map length . group . sort
      counts = map countNums $ lines text
      twos = length $ filter (Set.member 2) counts
      threes = length $ filter (Set.member 3) counts
   in twos * threes

part2 :: String -> Int
part2 _ = 2
