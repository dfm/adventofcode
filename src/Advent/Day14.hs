module Advent.Day14 (part1, part2) where

import Data.Bits (Bits (shiftL), complement, (.&.), (.|.))
import qualified Data.IntMap as IntMap
import Data.Maybe (catMaybes)
import Text.Regex.TDFA ((=~))

part1 :: Bool -> String -> Int
part1 _ = doPart parseState1 applyUpdate1

part2 :: Bool -> String -> Int
part2 _ = doPart parseState2 applyUpdate2

doPart :: StateFunc -> UpdateFunc -> String -> Int
doPart parseState applyUpdate =
  let procLine = parseLine parseState applyUpdate
      machine = Machine {mMemory = IntMap.empty, mMask = [id]}
   in sum . mMemory . foldl procLine machine . lines

-- Types
data Machine = Machine {mMemory :: IntMap.IntMap Int, mMask :: [Int -> Int]}

type StateFunc = ((Int, Char) -> Maybe [Int -> Int])

type UpdateFunc = Machine -> (Int, Int) -> Machine

-- Helpers to flip bits
oneBit :: Int -> (Int -> Int)
oneBit k = (.|. (1 `shiftL` k))

zeroBit :: Int -> (Int -> Int)
zeroBit k = (.&. complement (1 `shiftL` k))

-- Parse a mask instruction into a list of bit flip operations
parseMask :: StateFunc -> String -> [Int -> Int]
parseMask func text =
  let funcs = sequence $ catMaybes $ zipWith (curry func) [0 ..] (reverse text)
   in map (foldl (flip (.)) id) funcs

-- Parse a memory address and value
parseMem :: String -> (Int, Int)
parseMem text =
  let (_, _, _, inds) = text =~ "mem\\[([0-9]+)\\] = ([0-9]+)" :: (String, String, String, [String])
   in (read (head inds), read (inds !! 1))

-- Process a line and update the current machine
parseLine :: StateFunc -> UpdateFunc -> Machine -> String -> Machine
parseLine parseState applyUpdate machine@(Machine mem _) line
  | take 4 line == "mask" = Machine mem $ parseMask parseState (drop 7 line)
  | otherwise = applyUpdate machine $ parseMem line

-- The specific code for parts 1 and 2
parseState1 :: StateFunc
parseState1 (k, c)
  | c == '1' = Just [oneBit k]
  | c == '0' = Just [zeroBit k]
  | otherwise = Nothing

parseState2 :: StateFunc
parseState2 (k, c)
  | c == '1' = Just [oneBit k]
  | c == '0' = Nothing
  | otherwise = Just [zeroBit k, oneBit k]

applyUpdate1 :: UpdateFunc
applyUpdate1 (Machine mem mask) (add, val) =
  Machine (IntMap.insert add (head mask val) mem) mask

applyUpdate2 :: UpdateFunc
applyUpdate2 (Machine mem mask) (add, val) =
  Machine (foldl (\m f -> IntMap.insert (f add) val m) mem mask) mask
