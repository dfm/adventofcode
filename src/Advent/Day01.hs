module Advent.Day01 (part1, part2) where

import Data.List (tails)

part1 :: Bool -> String -> Int
part1 _ text = head [x * y | (x : ys) <- tails numbers, y <- ys, x + y == 2020]
  where
    numbers = parseInts text

part2 :: Bool -> String -> Int
part2 _ text = head [x * y * z | (x : ys) <- tails numbers, (y : zs) <- tails ys, z <- zs, x + y + z == 2020]
  where
    numbers = parseInts text

parseInts :: String -> [Int]
parseInts text = map read (lines text)