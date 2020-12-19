module Advent.Day13 (part1, part2) where

import Data.List.Split (splitOn)
import Data.Sort (sortOn)

part1 :: Bool -> String -> Int
part1 _ text =
  let [row1, row2] = lines text
      time = read row1
      schedule = [read x | x <- splitOn "," row2, x /= "x"] :: [Int]
      wrap x = x - time `mod` x
      result = head $ sortOn wrap schedule
   in result * wrap result

part2 :: Bool -> String -> Int
part2 _ text =
  let [_, schedule] = lines text
      pairs = [(n, read v) | (n, v) <- zip [0 ..] (splitOn "," schedule), v /= "x"]
      result = foldl computeNext (0, 1) pairs
   in fst result

computeNext :: (Int, Int) -> (Int, Int) -> (Int, Int)
computeNext (current, delta) (offset, modBy)
  | (current + offset) `mod` modBy == 0 = (current, delta * modBy)
  | otherwise = computeNext (current + delta, delta) (offset, modBy)
