module Advent.Day01 (part1, part2) where

import Data.List (sort, tails)
import qualified Data.Set as Set

part1 :: [Integer] -> Integer
part1 numbers = head $ take 1 [x * (2020 - x) | (x : ys) <- tails $ sort numbers, Set.member (2020 - x) $ Set.fromAscList ys]

part2 :: [Integer] -> Integer
part2 numbers = head $ take 1 [x * y * (2020 - x - y) | (x : ys) <- tails $ sort numbers, (y : zs) <- tails ys, Set.member (2020 - x - y) $ Set.fromAscList zs]
