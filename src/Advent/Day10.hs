module Advent.Day10 (part1, part2) where

import Data.IntMap ((!))
import qualified Data.IntMap as Map
import Data.Sort (sort)

part1 :: Bool -> String -> Int
part1 _ text =
  let numbers = loadData text
      d = [y - x | (x, y) <- zip (init numbers) (tail numbers)]
      one = length $ filter (== 1) d
      three = length $ filter (== 3) d
   in one * three

part2 :: Bool -> String -> Int
part2 _ text =
  let numbers = loadData text
      cache = foldl doUpdate (Map.singleton 0 1) (tail numbers)
   in cache ! last numbers

doUpdate :: Map.IntMap Int -> Int -> Map.IntMap Int
doUpdate cache n = Map.insert n (sum [Map.findWithDefault 0 (n + d) cache | d <- [-3, -2, -1]]) cache

loadData :: String -> [Int]
loadData text =
  let numbers = sort $ map read (lines text)
   in 0 : numbers ++ [last numbers + 3]
