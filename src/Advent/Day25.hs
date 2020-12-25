module Advent.Day25 (part1, part2) where

part1 :: Bool -> String -> Int
part1 _ text =
  let [card, door] = map read (lines text)
      cardLoop = findLoopSize 7 card
   in encrypt door cardLoop

part2 :: Bool -> String -> Int
part2 _ _ = 49

step :: Int -> Int -> Int
step subj = (`rem` 20201227) . (* subj)

encrypt :: Int -> Int -> Int
encrypt subj loopSize = foldr ($) 1 (replicate loopSize (step subj))

findLoopSize :: Int -> Int -> Int
findLoopSize subj key = fst . head $ filter ((== key) . snd) (zip [0 ..] $ iterate (step subj) 1)
