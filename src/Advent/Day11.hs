module Advent.Day11 where

import Data.Vector ((!))
import qualified Data.Vector as Vector

part1 :: Bool -> String -> Int
part1 _ = countTaken . findStationary partOneRules 0 . parseData

part2 :: Bool -> String -> Int
part2 _ = countTaken . findStationary partTwoRules 0 . parseData

-- Part 1 rules
partOneRules :: Seating -> Int -> (Bool, Seat)
partOneRules (Seating width seats) index
  | current == Empty = if numTaken == 0 then (True, Taken) else (False, Empty)
  | current == Taken = if numTaken >= 4 then (True, Empty) else (False, Taken)
  | otherwise = (False, current)
  where
    current = seats ! index
    adj = [seats ! (index + delta) | delta <- [- width - 1, - width, - width + 1, -1, 1, width -1, width, width + 1]]
    numTaken = length $ filter (== Taken) adj

-- Part 2 rules
getSeatInDirection :: Seating -> (Int, Int) -> Int -> Seat
getSeatInDirection x@(Seating width seats) (dx, dy) index
  | qIndex < 0 || qIndex >= length seats = Floor
  | qx == 0 || qx == width - 1 = Floor
  | query == Floor = getSeatInDirection x (dx, dy) qIndex
  | otherwise = query
  where
    qIndex = index + dx + width * dy
    qx = qIndex `mod` width
    query = seats ! qIndex

partTwoRules :: Seating -> Int -> (Bool, Seat)
partTwoRules seats index
  | current == Empty = if numTaken == 0 then (True, Taken) else (False, Empty)
  | current == Taken = if numTaken >= 5 then (True, Empty) else (False, Taken)
  | otherwise = (False, current)
  where
    current = sSeats seats ! index
    adj = [getSeatInDirection seats (dx, dy) index | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]
    numTaken = length $ filter (== Taken) adj

-- Seat manipulation
countTaken :: Seating -> Int
countTaken = length . Vector.filter (== Taken) . sSeats

doUpdate :: (Seating -> Int -> (Bool, Seat)) -> Seating -> (Bool, Seating)
doUpdate rules x@(Seating width seats) =
  let (changed, newSeats) = unzip $ map (rules x) [0 .. Vector.length seats - 1]
   in (or changed, Seating width $ Vector.fromList newSeats)

findStationary :: (Seating -> Int -> (Bool, Seat)) -> Int -> Seating -> Seating
findStationary rules current seats =
  let (changed, newSeats) = doUpdate rules seats
   in if changed then findStationary rules (current + 1) newSeats else seats

-- Parsing
data Seat = Floor | Empty | Taken deriving (Eq)

data Seating = Seating {sWidth :: Int, sSeats :: Vector.Vector Seat} deriving (Eq)

instance Show Seat where
  show seat = [showSeat seat]

instance Show Seating where
  show (Seating width seats) = showHelper width (Vector.toList seats)

showHelper :: Int -> [Seat] -> String
showHelper _ [] = []
showHelper width seats =
  let row = take width (map showSeat seats) ++ "\n"
   in row ++ showHelper width (drop width seats)

showSeat :: Seat -> Char
showSeat Floor = '.'
showSeat Empty = 'L'
showSeat Taken = '#'

parseChar :: Char -> Seat
parseChar '.' = Floor
parseChar 'L' = Empty
parseChar '#' = Taken
parseChar _ = error "Invalid character"

parseRow :: String -> [Seat]
parseRow row = Floor : map parseChar row ++ [Floor]

parseData :: String -> Seating
parseData text =
  let rows = map parseRow (lines text)
      width = length $ head rows
      padding = replicate width Floor
   in Seating width (Vector.fromList $ padding ++ concat rows ++ padding)
