module Advent.Day22 (part1, part2) where

import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

part1 :: Bool -> String -> Int
part1 _ = getScore . playTurns applyRules1 . parseInput

part2 :: Bool -> String -> Int
part2 _ = getScore . playTurns applyRules2 . parseInput

-- Game setup and dealing helpers
type Deck = Seq Int

data Game = Game
  { gOne :: Deck,
    gTwo :: Deck,
    gPrevious :: Set Int,
    gOneWins :: Bool
  }

pop :: Seq a -> (a, Seq a)
pop Seq.Empty = error "Empty deck"
pop (x :<| xs) = (x, xs)

push :: Seq a -> [a] -> Seq a
push c = (Seq.><) c . Seq.fromList

-- I/O
parseInput :: String -> Game
parseInput text =
  let [a, b] = splitOn "\n\n" text
      f = Seq.fromList . map read . tail . lines
   in Game
        { gOne = f a,
          gTwo = f b,
          gPrevious = Set.empty,
          gOneWins = False
        }

-- Hashing lists of ints
hashPair :: Int -> Int -> Int
hashPair k1 k2 = sm * (sm + 1) `div` 2 + k2
  where
    sm = k1 + k2

hashDeck :: Seq Int -> Int
hashDeck = foldl hashPair 0

-- Scoring
getScore :: Game -> Int
getScore (Game one _ _ True) = computeScore one
getScore (Game _ two _ False) = computeScore two

computeScore :: Seq Int -> Int
computeScore = sum . zipWith (*) [1 ..] . toList . Seq.reverse

-- Gameplay
playTurns :: (Int -> Game -> Game) -> Game -> Game
playTurns _ (Game Seq.Empty two _ _) = Game Seq.empty two Set.empty False
playTurns _ (Game one Seq.Empty _ _) = Game one Seq.empty Set.empty True
playTurns rules game@(Game one two prev _)
  | Set.member state prev = Game one two Set.empty True
  | otherwise = rules state game
  where
    state = hashPair (hashDeck one) (hashDeck two)

-- Rule sets
applyRules1 :: Int -> Game -> Game
applyRules1 _ (Game one two prev _) =
  let (one', two') = compareHighCard (pop one) (pop two)
   in playTurns applyRules1 (Game one' two' prev False)

applyRules2 :: Int -> Game -> Game
applyRules2 state (Game one two prev _) =
  let prev' = Set.insert state prev
      (one', two') = recurseGame (pop one) (pop two)
   in playTurns applyRules2 (Game one' two' prev' False)

-- Deck update helpers
compareHighCard :: (Int, Deck) -> (Int, Deck) -> (Deck, Deck)
compareHighCard (a, one) (b, two) =
  let one' = if a > b then push one [a, b] else one
      two' = if a <= b then push two [b, a] else two
   in (one', two')

recurseGame :: (Int, Deck) -> (Int, Deck) -> (Deck, Deck)
recurseGame (a, one) (b, two)
  | a <= length one && b <= length two =
    let (Game _ _ _ flag) = playTurns applyRules2 (Game (Seq.take a one) (Seq.take b two) Set.empty False)
        one' = if flag then push one [a, b] else one
        two' = if not flag then push two [b, a] else two
     in (one', two')
  | otherwise = compareHighCard (a, one) (b, two)
