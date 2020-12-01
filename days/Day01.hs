module Main where

import Advent (loadListOfInts)
import Advent.Day01 (part1, part2)

main :: IO ()
main = do
  numbers <- loadListOfInts "inputs/day01"
  putStrLn "Day 1:"
  putStr " - Part 1: "
  print $ part1 numbers
  putStr " - Part 2: "
  print $ part2 numbers
