module Main where

import Advent (loadListOfInts)
import Advent.Day01 (part1, part2)

main :: IO ()
main = do
  numbers <- loadListOfInts "inputs/day01"
  print $ part1 numbers
  print $ part2 numbers
