module Advent.Day01 (part1, part2) where

import Data.List (tails)

part1 :: String -> Integer
part1 text = head [x * y | (x : ys) <- tails numbers, y <- ys, x + y == 2020]
  where
    numbers = parseInts text

part2 :: String -> Integer
part2 text = head [x * y * z | (x : ys) <- tails numbers, (y : zs) <- tails ys, z <- zs, x + y + z == 2020]
  where
    numbers = parseInts text

parseInts :: String -> [Integer]
parseInts text = map read (lines text)