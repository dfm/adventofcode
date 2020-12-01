module Advent (parseInts) where

parseInts :: String -> [Integer]
parseInts = parseInt . lines

parseInt :: [String] -> [Integer]
parseInt = map read
