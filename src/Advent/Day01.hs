module Advent.Day01 (part1, part2) where

import Data.List (sort, tails)

findFirst :: (Ord a, Num a) => a -> [a] -> [a]
findFirst x sortedNumbers = take 1 [y | y <- sortedNumbers, x + y >= 2020]

part1 :: [Integer] -> Integer
part1 numbers = head $ take 1 [x * y | (x : ys) <- tails $ sort numbers, y <- findFirst x ys, x + y == 2020]

part2 :: [Integer] -> Integer
part2 numbers = head $ take 1 [x * y * z | (x : ys) <- tails $ sort numbers, (y : zs) <- tails ys, z <- findFirst (x + y) zs, x + y + z == 2020]
