module Advent.Day15 where

import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)

part1 :: Bool -> String -> Int
part1 _ text =
  let input = parseInput text
      result = foldl' (\state _ -> getNextNumber state) input [sTurn input .. 2020 - 1]
   in sNumber result

part2 :: Bool -> String -> Int
part2 _ text =
  let input = parseInput text
      result = foldl' (\state _ -> getNextNumber state) input [sTurn input .. 30000000 - 1]
   in sNumber result

data State = State {sTurn :: Int, sNumber :: Int, sPast :: IntMap Int} deriving (Show)

parseInput :: String -> State
parseInput text =
  let numbers = map read $ splitOn "," text
   in State
        { sTurn = length numbers,
          sNumber = last numbers,
          sPast = IM.fromList $ zip (init numbers) [0 ..]
        }

getNextNumber :: State -> State
getNextNumber (State turn current past) =
  let n = if IM.member current past then turn - 1 - past IM.! current else 0
      newPast = IM.insert current (turn - 1) past
   in State {sTurn = turn + 1, sNumber = n, sPast = newPast}
