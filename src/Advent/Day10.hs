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
      tree = Map.fromList $ buildTree numbers
      cache = foldr (countPaths tree) (Map.singleton (last numbers) 1) (init numbers)
   in cache ! 0

loadData :: String -> [Int]
loadData text =
  let numbers = sort $ map read (lines text)
   in 0 : numbers ++ [last numbers + 3]

buildTree :: [Int] -> [(Int, [Int])]
buildTree (x : xs) = (x, takeWhile (<= x + 3) xs) : buildTree xs
buildTree [] = []

countPaths :: Map.IntMap [Int] -> Int -> Map.IntMap Int -> Map.IntMap Int
countPaths tree node cache = Map.insert node (sum $ map (cache !) (tree ! node)) cache
