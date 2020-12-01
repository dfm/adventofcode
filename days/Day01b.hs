module Main where

import Data.List (tails)

main :: IO ()
main = do
  contents <- getContents
  let numbers = parseInt (lines contents)
      results = [x * y * z | (x : ys) <- tails numbers, (y : zs) <- tails ys, z <- zs, x + y + z == 2020]
  print $ head results

parseInt :: [String] -> [Integer]
parseInt = map read