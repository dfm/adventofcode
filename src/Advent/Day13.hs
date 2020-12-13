module Advent.Day13 where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)

part1 :: Bool -> String -> Int
part1 _ text =
  let (time, schedule) = parseInput (lines text)
      inService = catMaybes schedule
      comp = getWaitTime time
      first = minimumBy (\x y -> compare (comp x) (comp y)) inService
   in fromIntegral (first * comp first)

part2 :: Bool -> String -> Int
part2 _ text =
  let (_, schedule) = parseInput (lines text)
      system = mapMaybe convertToMod $ zip [0 ..] schedule
   in fromIntegral (fst $ foldl1 solve system)

convertToMod :: (Integer, Maybe Integer) -> Maybe (Integer, Integer)
convertToMod (_, Nothing) = Nothing
convertToMod (n, Just x) = Just (wrap x (- n), x)

parseSchedule :: String -> [Maybe Integer]
parseSchedule = map readMaybe . splitOn ","

parseInput :: [String] -> (Integer, [Maybe Integer])
parseInput [time, schedule] = (read time, parseSchedule schedule)
parseInput _ = error "Invalid input"

getWaitTime :: Integer -> Integer -> Integer
getWaitTime earliest number = number - earliest `mod` number

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
extendedGcd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
extendedGcd (r, r') (s, s')
  | r' == 0 = (r, s)
  | otherwise =
    let q = r `div` r'
     in extendedGcd (r', r - q * r') (s', s - q * s')

-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
solve :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solve (a1, n1) (a2, n2) =
  let (_, m1) = extendedGcd (n1, n2) (1, 0)
      n = n1 * n2
   in (wrap n (a1 + (a2 - a1) * m1 * n1), n)

wrap :: Integer -> Integer -> Integer
wrap n x
  | x < 0 = wrap n (x + n)
  | otherwise = x `mod` n
