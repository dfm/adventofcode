module Advent.Day22 where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

part1 :: Bool -> String -> Int
part1 _ = getScore . playTurns applyRules1 . parseInput

part2 :: Bool -> String -> Int
part2 _ = getScore . playTurns applyRules2 . parseInput

data Game = Game
  { gOne :: [Int],
    gTwo :: [Int],
    gPrevious :: Set Int,
    gOneWins :: Bool
  }

pop :: [a] -> (a, [a])
pop [] = error "Empty deck"
pop (c : cs) = (c, cs)

push :: [a] -> [a] -> [a]
push c cs = cs ++ c

-- I/O
parseInput :: String -> Game
parseInput text =
  let [a, b] = splitOn "\n\n" text
      f = map read . tail . lines
   in Game
        { gOne = f a,
          gTwo = f b,
          gPrevious = S.empty,
          gOneWins = False
        }

-- Hashing lists of ints
hashPair :: Int -> Int -> Int
hashPair k1 k2 = sm * (sm + 1) `div` 2 + k2
  where
    sm = k1 + k2

hashDeck :: [Int] -> Int
hashDeck = foldl hashPair 0

-- Gameplay
computeScore :: [Int] -> Int
computeScore = sum . zipWith (*) [1 ..] . reverse

getScore :: Game -> Int
getScore (Game one _ _ True) = computeScore one
getScore (Game _ two _ False) = computeScore two

playTurns :: (Int -> Game -> Game) -> Game -> Game
playTurns _ (Game [] two _ _) = Game [] two S.empty False
playTurns _ (Game one [] _ _) = Game one [] S.empty True
playTurns rules game@(Game one two prev _)
  | S.member state prev = Game one two S.empty True
  | otherwise = rules state game
  where
    state = hashPair (hashDeck one) (hashDeck two)

compareHighCard :: (Int, [Int]) -> (Int, [Int]) -> ([Int], [Int])
compareHighCard (a, one) (b, two) =
  let one' = if a > b then push [a, b] one else one
      two' = if a <= b then push [b, a] two else two
   in (one', two')

applyRules1 :: Int -> Game -> Game
applyRules1 _ (Game one two prev _) =
  let (one', two') = compareHighCard (pop one) (pop two)
   in playTurns applyRules1 (Game one' two' prev False)

recurseGame :: (Int, [Int]) -> (Int, [Int]) -> ([Int], [Int])
recurseGame (a, one) (b, two)
  | a <= length one && b <= length two =
    let (Game _ _ _ flag) = playTurns applyRules2 (Game (take a one) (take b two) S.empty False)
        one' = if flag then push [a, b] one else one
        two' = if not flag then push [b, a] two else two
     in (one', two')
  | otherwise = compareHighCard (a, one) (b, two)

applyRules2 :: Int -> Game -> Game
applyRules2 state (Game one two prev _) =
  let prev' = S.insert state prev
      (one', two') = recurseGame (pop one) (pop two)
   in playTurns applyRules2 (Game one' two' prev' False)