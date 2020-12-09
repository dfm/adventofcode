module Advent.Day06 (part1, part2) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

part1 :: Bool -> String -> Int
part1 _ = sum . map (Set.size . foldl1 Set.union) . getGroups

part2 :: Bool -> String -> Int
part2 _ = sum . map (Set.size . foldl1 Set.intersection) . getGroups

getGroups :: String -> [[Set.Set Char]]
getGroups = map (map Set.fromList . lines) . splitOn "\n\n"
