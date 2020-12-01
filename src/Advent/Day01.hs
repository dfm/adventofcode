module Advent.Day01 (parseInts, part1, part2) where

import Data.List (tails)

parseInts :: String -> [Integer]
parseInts = parseInt . lines

parseInt :: [String] -> [Integer]
parseInt = map read

part1 :: [Integer] -> Integer
part1 numbers = head [x * y | (x : ys) <- tails numbers, y <- ys, x + y == 2020]

part2 :: [Integer] -> Integer
part2 numbers = head [x * y * z | (x : ys) <- tails numbers, (y : zs) <- tails ys, z <- zs, x + y + z == 2020]
