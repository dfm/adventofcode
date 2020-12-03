module Advent.Day03 (part1, part2) where

data TreeMap = TreeMap Int Int [Bool] deriving (Show)

part1 :: String -> Int
part1 inp =
  let dx = 3
      dy = 1
      treeMap = readTreeMap inp
   in move treeMap dx dy 0 0 0

part2 :: String -> Int
part2 inp =
  let treeMap = readTreeMap inp
   in product $ [move treeMap dx dy 0 0 0 | (dx, dy) <- zip [1, 3, 5, 7, 1] [1, 1, 1, 1, 2]]

readTreeMap :: String -> TreeMap
readTreeMap text =
  let inp = lines text
      rows = length inp
      cols = length $ head inp
   in TreeMap rows cols $ map (== '#') (concat inp)

move :: TreeMap -> Int -> Int -> Int -> Int -> Int -> Int
move tree@(TreeMap rows cols inp) dx dy x y count
  | y >= rows = count
  | otherwise = move tree dx dy ((x + dx) `mod` cols) (y + dy) newCount
  where
    ind = x + y * cols
    newCount = if inp !! ind then count + 1 else count
