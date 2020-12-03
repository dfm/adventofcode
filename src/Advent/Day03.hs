module Advent.Day03 (part1, part2) where

newtype TreeMap = TreeMap (Int, [Bool]) deriving (Show)

part1 :: String -> Int
part1 inp =
  let TreeMap (stride, vs) = readTreeMap inp
   in countTrees vs stride 3 1

part2 :: String -> Int
part2 inp =
  let TreeMap (stride, vs) = readTreeMap inp
   in product $ [countTrees vs stride dx dy | (dx, dy) <- zip [1, 3, 5, 7, 1] [1, 1, 1, 1, 2]]

readTreeMap :: String -> TreeMap
readTreeMap text =
  let inp = lines text
      cols = length $ head inp
   in TreeMap (cols, map (== '#') (concat inp))

countTrees :: [Bool] -> Int -> Int -> Int -> Int
countTrees vs stride dx dy = length $ filter id (sled vs stride dx dy 0 0)

sled :: [Bool] -> Int -> Int -> Int -> Int -> Int -> [Bool]
sled [] _ _ _ _ _ = []
sled (v : vs) stride dx dy x y =
  let x' = (x + dx) `mod` stride
      y' = y + dy
      delta = x' - x + dy * stride - 1
   in v : sled (drop delta vs) stride dx dy x' y'
