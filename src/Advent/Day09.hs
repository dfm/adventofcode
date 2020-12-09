module Advent.Day09 (part1, part2) where

import Data.List (tails)

part1 :: Bool -> String -> Int
part1 isTest = findInvalidNumber (getPreambleSize isTest) . loadData

part2 :: Bool -> String -> Int
part2 isTest text =
  let numbers = loadData text
      invalid = findInvalidNumber (getPreambleSize isTest) numbers
      cs = scanl (+) 0 numbers
      enumerate = zip [0 ..] cs
      (n, m) = head [(i, j) | ((i, x) : xs) <- tails enumerate, (j, y) <- xs, y - x == invalid]
      mn = minimum $ take (m - n) (drop n numbers)
      mx = maximum $ take (m - n) (drop n numbers)
   in mn + mx

getPreambleSize :: Bool -> Int
getPreambleSize True = 5
getPreambleSize False = 25

doCheck :: [Int] -> Int -> Bool
doCheck preamble number = or [x + y == number | (x : xs) <- tails preamble, y <- xs]

runChecks :: [Int] -> Int -> [Int] -> [Bool]
runChecks preamble number (x : xs) = doCheck preamble number : runChecks (number : init preamble) x xs
runChecks preamble number [] = [doCheck preamble number]

findInvalidNumber :: Int -> [Int] -> Int
findInvalidNumber preambleSize numbers =
  let preamble = reverse $ take preambleSize numbers
      (number : rest) = drop preambleSize numbers
      checks = runChecks preamble number rest
   in fst (head $ filter (not . snd) (zip (number : rest) checks))

loadData :: String -> [Int]
loadData = map read . lines
