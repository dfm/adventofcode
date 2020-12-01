module Main where

import Data.List (tails)

main :: IO ()
main = do
  contents <- getContents
  let numbers = parseInt (lines contents)
      results = [x * y | (x : ys) <- tails numbers, y <- ys, x + y == 2020]
  print $ head results

parseInt :: [String] -> [Integer]
parseInt = map read