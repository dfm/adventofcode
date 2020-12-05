module Main where

import Advent (runPart)
import Advent.Day05 (part1, part2)

main :: IO ()
main = do
  result1 <- runPart "inputs/day05" part1
  print result1
  result2 <- runPart "inputs/day05" part2
  print result2
