module Advent.Day02 where

import Advent.Solver (Solver (..))
import Data.List.Split (splitOn)
import Data.Sort (sort)

day02a :: Solver [(Int, Int, Int)] Int
day02a =
  Solver
    { sParse = Just . map parse . lines,
      sSolve = Just . sum . map paper,
      sShow = show
    }

day02b :: Solver [(Int, Int, Int)] Int
day02b =
  Solver
    { sParse = Just . map parse . lines,
      sSolve = Just . sum . map ribbon,
      sShow = show
    }

parse :: String -> (Int, Int, Int)
parse text =
  let [x, y, z] = sort $ take 3 (map read $ splitOn "x" text)
   in (x, y, z)

paper :: (Int, Int, Int) -> Int
paper (x, y, z) = 2 * (x * y + x * z + y * z) + x * y

ribbon :: (Int, Int, Int) -> Int
ribbon (x, y, z) = 2 * (x + y) + x * y * z
