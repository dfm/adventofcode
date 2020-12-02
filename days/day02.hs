module Main where

import Advent.Day02 (part1, part2)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

main :: IO ()
main = do
  withFile
    "inputs/day02"
    ReadMode
    ( \handle -> do
        text <- hGetContents handle
        print $ part1 (lines text)
        print $ part2 (lines text)
    )
