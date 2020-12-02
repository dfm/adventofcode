module Main where

import Advent (loadListOfInts)
import Advent.Day01 (part1, part2)
import System.IO (IOMode (ReadMode), withFile)

main :: IO ()
main = do
  withFile
    "inputs/day01"
    ReadMode
    ( \handle -> do
        numbers <- loadListOfInts handle
        print $ part1 numbers
        print $ part2 numbers
    )
