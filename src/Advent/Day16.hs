module Advent.Day16 (part1, part2) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

part1 :: Bool -> String -> Int
part1 _ text =
  let (Input f _ t) = parseInput text
   in sum $ concatMap (getInvalidFields f) t

part2 :: Bool -> String -> Int
part2 isTest text =
  let inp = parseInput text
      assign = assignInput inp
      entries = filter (\(n, _) -> isPrefixOf (getPrefix isTest) $ fName (assign IM.! n)) $ zip [0 ..] (iMine inp)
   in foldl (\acc (_, x) -> acc * x) 1 entries

-- Helper for running on test input
getPrefix :: Bool -> String
getPrefix True = "seat"
getPrefix False = "departure"

-- Parsing
type RegexMatch = (String, String, String, [String])

data Field = Field {fName :: String, fRanges :: [(Int, Int)]} deriving (Show)

data Input = Input {iFields :: [Field], iMine :: [Int], iNearby :: [[Int]]} deriving (Show)

makePair :: [String] -> [(Int, Int)]
makePair [] = []
makePair (a : b : xs) = (read a, read b) : makePair xs
makePair [_] = error "Invalid input"

parseField :: String -> Field
parseField text =
  let (_, _, _, name : xs) = text =~ "(.+): ([0-9]+)\\-([0-9]+) or ([0-9]+)\\-([0-9]+)" :: RegexMatch
   in Field {fName = name, fRanges = makePair xs}

parseFields :: String -> [Field]
parseFields = map parseField . lines

parseTickets :: String -> [[Int]]
parseTickets = map (map read . splitOn ",") . drop 1 . lines

parseInput :: String -> Input
parseInput text =
  let [a, b, c] = splitOn "\n\n" text
   in Input {iFields = parseFields a, iMine = head (parseTickets b), iNearby = parseTickets c}

-- Checking validity of fields
fieldIsValid :: Int -> Field -> Bool
fieldIsValid n (Field _ ranges) = any (\(mn, mx) -> mn <= n && n <= mx) ranges

getInvalidFields :: [Field] -> [Int] -> [Int]
getInvalidFields f = filter (\n -> not $ any (fieldIsValid n) f)

-- Discarding invalid tickets
onlyValid :: [Field] -> [[Int]] -> [[Int]]
onlyValid f = filter (null . getInvalidFields f)

-- Checking validity of columns
transposeList :: [[Int]] -> [[Int]]
transposeList ([] : _) = []
transposeList x = map head x : transposeList (map tail x)

columnIsValid :: Field -> [Int] -> Bool
columnIsValid f = all (`fieldIsValid` f)

-- Assign the place for fields where there is only one valid assignment
assignPlace :: (Field, [Bool]) -> (IntMap Field, [(Field, [Bool])]) -> (IntMap Field, [(Field, [Bool])])
assignPlace (f, v) (soFar, todo) =
  let best : rest = filter (\(n, x) -> x && not (IM.member n soFar)) $ zip [0 ..] v
   in if null rest then (IM.insert (fst best) f soFar, todo) else (soFar, (f, v) : todo)

-- Iterate assignment, removing previously assigned places until finished
assignPlaces :: (IntMap Field, [(Field, [Bool])]) -> IntMap Field
assignPlaces (done, []) = done
assignPlaces (soFar, todo) = assignPlaces $ foldr assignPlace (soFar, []) todo

-- Helper to run assignPlaces on a given input
assignInput :: Input -> IntMap Field
assignInput (Input f m t) =
  let allowed = map (\x -> map (columnIsValid x) (transposeList (m : onlyValid f t))) f
   in assignPlaces (IM.empty, zip f allowed)
