module Advent.Day01 where

import Advent.Solver (Solver (..))

day01a :: Solver [Int] Int
day01a =
  Solver
    { sParse = mapM parse,
      sSolve = Just . sum,
      sShow = show
    }

day01b :: Solver [Int] Int
day01b =
  Solver
    { sParse = mapM parse,
      sSolve = Just . fst . head . filter ((< 0) . snd) . zip [0 ..] . scanl (+) 0,
      sShow = show
    }

parse :: Char -> Maybe Int
parse '(' = Just 1
parse ')' = Just (-1)
parse _ = Nothing
