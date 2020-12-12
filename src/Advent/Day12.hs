module Advent.Day12 (part1, part2) where

part1 :: Bool -> String -> Int
part1 _ = doPart move1 (1, 0)

part2 :: Bool -> String -> Int
part2 _ = doPart move2 (10, 1)

doPart :: (Char -> Int -> Pointing -> Pointing) -> (Int, Int) -> String -> Int
doPart mover start = distance . foldl (runInstr mover) (Pointing (0, 0) start) . lines

data Pointing = Pointing (Int, Int) (Int, Int) deriving (Show)

-- Manhattan distance
distance :: Pointing -> Int
distance (Pointing (x, y) _) = abs x + abs y

-- Rotation matrices
rotate :: Int -> Pointing -> Pointing
rotate amt p@(Pointing (x, y) (dx, dy))
  | r == 1 = Pointing (x, y) (dy, - dx)
  | r == 2 = Pointing (x, y) (- dx, - dy)
  | r == 3 = Pointing (x, y) (- dy, dx)
  | otherwise = p
  where
    r = (amt `div` 90) `mod` 4

-- Moving rules
baseMove :: Char -> Int -> (Int, Int) -> (Int, Int)
baseMove dir value (x, y)
  | dir == 'N' = (x, y + value)
  | dir == 'E' = (x + value, y)
  | dir == 'S' = (x, y - value)
  | dir == 'W' = (x - value, y)
  | otherwise = error $ "Invalid direction: " ++ [dir]

move1 :: Char -> Int -> Pointing -> Pointing
move1 dir value (Pointing x dx) = Pointing (baseMove dir value x) dx

move2 :: Char -> Int -> Pointing -> Pointing
move2 dir value (Pointing x dx) = Pointing x (baseMove dir value dx)

execute :: Int -> Pointing -> Pointing
execute amt (Pointing (x, y) (dx, dy)) = Pointing (x + amt * dx, y + amt * dy) (dx, dy)

runInstr :: (Char -> Int -> Pointing -> Pointing) -> Pointing -> String -> Pointing
runInstr mover p (dir : amt)
  | dir == 'R' = rotate value p
  | dir == 'L' = rotate (- value) p
  | dir == 'F' = execute value p
  | otherwise = mover dir value p
  where
    value = read amt
runInstr _ _ [] = error "Invalid instruction"
