{-# LANGUAGE TupleSections #-}

module Advent.Day21 (part1, part2) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sort (sortOn)

part1 :: Bool -> String -> Int
part1 _ text =
  let x = map parseLine (lines text)
      w = concatMap fst x
      d = foldl convertToDict M.empty x
      a = foldl1 S.union (M.elems d)
   in length $ filter (not . (`S.member` a)) w

part2 :: Bool -> String -> String
part2 _ text =
  let x = map parseLine (lines text)
      d = foldl convertToDict M.empty x
      l = sortOn fst $ M.assocs (label d M.empty)
   in intercalate "," $ map (S.elemAt 0 . snd) l

parseLine :: String -> ([String], [String])
parseLine text =
  let [a, b] = splitOn " (contains " text
   in (words a, splitOn ", " (init b))

convertToDict :: Map String (Set String) -> ([String], [String]) -> Map String (Set String)
convertToDict d (i, a) =
  let s = S.fromList i
      d' = M.fromList $ map (,s) a
   in M.unionWith S.intersection d' d

getKnown :: String -> Set String -> Map String (Set String) -> Map String (Set String)
getKnown a i m
  | S.size i == 1 = M.insert a i m
  | otherwise = m

label :: Map String (Set String) -> Map String (Set String) -> Map String (Set String)
label d k =
  let w = foldl S.union S.empty (M.elems k)
      d' = M.map (`S.difference` w) $ M.difference d k
      k' = M.foldrWithKey getKnown k d'
   in if M.null d' then k' else label d' k'
