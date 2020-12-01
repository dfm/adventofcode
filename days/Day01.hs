module Main where

import Advent
import Advent.Day01

main :: IO ()
main = do
  contents <- getContents
  let numbers = parseInts contents
  print $ part1 $ numbers
  print $ part2 $ numbers
