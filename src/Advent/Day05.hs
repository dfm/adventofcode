module Advent.Day05 (day05a, day05b) where

import Advent.Solver (Solver (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

day05a :: Solver [String] Int
day05a =
  Solver
    { sParse = Just . lines,
      sSolve = Just . length . filter checkWord,
      sShow = show
    }

day05b :: Solver [String] Int
day05b =
  Solver
    { sParse = Just . lines,
      sSolve = Just . length . filter checkWord2,
      sShow = show
    }

vowels :: Set Char
vowels = Set.fromList "aeiou"

bads :: Set String
bads = Set.fromList ["ab", "cd", "pq", "xy"]

checkRules :: (Int, Bool, Bool) -> String -> (Int, Bool, Bool)
checkRules (vows, dubs, bad) (a : b : cs) =
  let vows' = vows + fromEnum (Set.member a vowels)
      dubs' = dubs || (a == b)
      bad' = bad || Set.member [a, b] bads
   in checkRules (vows', dubs', bad') (b : cs)
checkRules (vows, dubs, bad) (a : cs) =
  checkRules (vows + fromEnum (Set.member a vowels), dubs, bad) cs
checkRules r _ = r

checkWord :: String -> Bool
checkWord text =
  let (vows, dubs, bad) = checkRules (0, False, False) text
   in (vows >= 3) && dubs && not bad

checkTrips :: String -> Bool
checkTrips (a : b : c : ds) = (a == c) || checkTrips (b : c : ds)
checkTrips _ = False

findDubs :: Int -> Map String [Int] -> String -> Map String [Int]
findDubs n m (a : b : cs) = findDubs (n + 1) (Map.unionWith (++) m (Map.singleton [a, b] [n])) (b : cs)
findDubs _ m _ = m

checkDubs :: String -> Bool
checkDubs word =
  let dubs = Map.elems $ findDubs 0 Map.empty word
      check d = maximum d > minimum d + 1
   in any check dubs

checkWord2 :: String -> Bool
checkWord2 = (&&) <$> checkTrips <*> checkDubs
