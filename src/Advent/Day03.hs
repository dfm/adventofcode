module Advent.Day03 where

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

-- move :: TreeMap -> Int -> Int -> Int
-- move (TreeMap _ _ []) _ count = count
-- move (TreeMap rows cols (el : rest)) delta count =
--   let newCount = if el then count + 1 else count
--    in move (TreeMap rows cols (take delta rest)) delta newCount

-- part2 :: String -> Int
-- part2 = part checkPasswordRule2

-- part :: (PasswordRule -> Bool) -> String -> Int
-- part checker text = length $ filter (checker . parseRule) (lines text)

-- parseRule :: String -> PasswordRule
-- parseRule text =
--   let [inds, c : _, word] = words text
--       [idx1, idx2] = splitOn "-" inds
--    in PasswordRule (read idx1) (read idx2) c word

-- checkPasswordRule :: PasswordRule -> Bool
-- checkPasswordRule (PasswordRule mn mx target word) =
--   let count = length $ filter (== target) word
--    in count >= mn && count <= mx

-- checkPasswordRule2 :: PasswordRule -> Bool
-- checkPasswordRule2 (PasswordRule idx1 idx2 target word) =
--   let at1 = word !! (idx1 - 1)
--       at2 = word !! (idx2 - 1)
--    in (at1 == target) /= (at2 == target)
