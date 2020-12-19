module Advent.Day13 (part1, part2) where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)
import Data.Sort (sortOn)

part1 :: Bool -> String -> Int
part1 _ text =
  let [row1, row2] = lines text
      time = read row1
      schedule = [read x | x <- splitOn "," row2, x /= "x"]
      result = minimumBy (comparing snd) [(x, x - time `mod` x) | x <- schedule]
   in uncurry (*) result

part2 :: Bool -> String -> Int
part2 _ text =
  let [_, schedule] = lines text
      pairs = sortOn (Down . snd) [(n, read v) | (n, v) <- zip [0 ..] (splitOn "," schedule), v /= "x"]
      result = foldl computeNext (0, 1) pairs
   in fst result

computeNext :: (Int, Int) -> (Int, Int) -> (Int, Int)
computeNext (current, delta) (offset, modBy)
  | (current + offset) `mod` modBy == 0 = (current, delta * modBy)
  | otherwise = computeNext (current + delta, delta) (offset, modBy)
