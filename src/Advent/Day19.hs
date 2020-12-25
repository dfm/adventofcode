-- Original regex version in Day19Original.hs
-- This version trying to understand
-- https://github.com/mstksg/advent-of-code-2020/blob/9193ef5babec896e0269e3fa3141db1068705766/src/AOC/Challenge/Day19.hs
{-# LANGUAGE DeriveFunctor #-}

module Advent.Day19 where

import Control.Monad (ap, (>=>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

part1 :: Bool -> String -> Int
part1 _ = runPart IntMap.empty

part2 :: Bool -> String -> Int
part2 _ =
  runPart
    ( IntMap.fromList
        [ (8, Compound (Or [Leaf 42, And [Leaf 42, Leaf 8]])),
          (11, Compound (Or [And [Leaf 42, Leaf 31], And [Leaf 42, Leaf 11, Leaf 31]]))
        ]
    )

runPart :: IntMap Rule -> String -> Int
runPart extraRules text =
  let (rules, messages) = readInput text
      expandedRules = expandRules (IntMap.union extraRules rules)
      matches = map (match (expandedRules IntMap.! 0)) messages
      count = length $ filter (any null) matches
   in count

data BaseRule a = Leaf a | And [BaseRule a] | Or [BaseRule a] deriving (Show, Eq, Ord, Functor)

data Rule = Simple Char | Compound (BaseRule Int) deriving (Show)

instance Applicative BaseRule where
  pure = return
  (<*>) = ap

instance Monad BaseRule where
  return = Leaf
  rule >>= f = case rule of
    Leaf x -> f x
    And xs -> And (map (>>= f) xs)
    Or xs -> Or (map (>>= f) xs)

readInput :: String -> (IntMap Rule, [String])
readInput text =
  let [rules, messages] = splitOn "\n\n" text
   in (foldl readRule IntMap.empty (lines rules), lines messages)

readRule :: IntMap Rule -> String -> IntMap Rule
readRule cache text =
  let (name, _, body) = text =~ ": " :: (String, String, String)
   in IntMap.insert (read name) (toRule body) cache

toRule :: String -> Rule
toRule ['"', c, '"'] = Simple c
toRule x = (Compound . Or) $ map (And . map (Leaf . read) . words) (splitOn " | " x)

expandRules :: IntMap Rule -> IntMap (BaseRule Char)
expandRules rules = res
  where
    res = fmap inner rules
    inner x = case x of
      Simple c -> Leaf c
      Compound cs -> cs >>= (res IntMap.!)

match :: BaseRule Char -> String -> [String]
match (Leaf _) [] = []
match (Leaf c) (x : xs)
  | x == c = [xs]
  | otherwise = []
match (And xs) text = foldr (>=>) pure (match <$> xs) text
match (Or xs) text = concatMap (`match` text) xs
