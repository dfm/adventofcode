module Advent.Day03 (day03a, day03b) where

import Advent.Solver (Solver (..))
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

day03a :: Solver [(Int, Int)] Int
day03a =
  Solver
    { sParse = mapM parse,
      sSolve = Just . Set.size . travel,
      sShow = show
    }

day03b :: Solver [(Int, Int)] Int
day03b =
  Solver
    { sParse = mapM parse,
      sSolve = Just . Set.size . roboTravel,
      sShow = show
    }

parse :: Char -> Maybe (Int, Int)
parse '>' = Just (1, 0)
parse 'v' = Just (0, -1)
parse '<' = Just (-1, 0)
parse '^' = Just (0, 1)
parse _ = Nothing

step :: ((Int, Int), Set (Int, Int)) -> (Int, Int) -> ((Int, Int), Set (Int, Int))
step ((x, y), past) (dx, dy) = ((x', y'), Set.insert (x', y') past)
  where
    x' = x + dx
    y' = y + dy

travel :: [(Int, Int)] -> Set (Int, Int)
travel = snd . foldl' step ((0, 0), Set.singleton (0, 0))

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x : xs) = x : everyOther (drop 1 xs)

roboTravel :: [(Int, Int)] -> Set (Int, Int)
roboTravel xs =
  let s1 = travel $ everyOther xs
      s2 = travel $ everyOther (drop 1 xs)
   in Set.union s1 s2