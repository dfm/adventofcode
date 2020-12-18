module Advent (runPart1, runPart2, getFileName, loadData, Day (..)) where

import qualified Advent.Day01
import qualified Advent.Day02
import qualified Advent.Day03
import qualified Advent.Day04
import qualified Advent.Day05
import qualified Advent.Day06
import qualified Advent.Day07
import qualified Advent.Day08
import qualified Advent.Day09
import qualified Advent.Day10
import qualified Advent.Day11
import qualified Advent.Day12
import qualified Advent.Day13
import qualified Advent.Day14
import qualified Advent.Day15
import qualified Advent.Day16
import qualified Advent.Day17
import qualified Advent.Day18
import qualified Advent.Day19
import qualified Data.ByteString as S
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

loadData :: FilePath -> IO String
loadData filename = do
  contents <- S.readFile filename
  let text = T.unpack $ E.decodeUtf8 contents
  return text

runPart :: (Bool -> String -> a) -> Bool -> FilePath -> IO a
runPart func isTest filename = do
  text <- loadData filename
  return $ func isTest text

data Day
  = Day01
  | Day02
  | Day03
  | Day04
  | Day05
  | Day06
  | Day07
  | Day08
  | Day09
  | Day10
  | Day11
  | Day12
  | Day13
  | Day14
  | Day15
  | Day16
  | Day17
  | Day18
  | Day19
  deriving (Enum, Bounded, Show, Read)

getFileName :: Day -> FilePath -> FilePath
getFileName day = (++ "/" ++ map toLower (show day))

runPart1 :: Day -> Bool -> FilePath -> IO Int
runPart1 Day01 = runPart Advent.Day01.part1
runPart1 Day02 = runPart Advent.Day02.part1
runPart1 Day03 = runPart Advent.Day03.part1
runPart1 Day04 = runPart Advent.Day04.part1
runPart1 Day05 = runPart Advent.Day05.part1
runPart1 Day06 = runPart Advent.Day06.part1
runPart1 Day07 = runPart Advent.Day07.part1
runPart1 Day08 = runPart Advent.Day08.part1
runPart1 Day09 = runPart Advent.Day09.part1
runPart1 Day10 = runPart Advent.Day10.part1
runPart1 Day11 = runPart Advent.Day11.part1
runPart1 Day12 = runPart Advent.Day12.part1
runPart1 Day13 = runPart Advent.Day13.part1
runPart1 Day14 = runPart Advent.Day14.part1
runPart1 Day15 = runPart Advent.Day15.part1
runPart1 Day16 = runPart Advent.Day16.part1
runPart1 Day17 = runPart Advent.Day17.part1
runPart1 Day18 = runPart Advent.Day18.part1
runPart1 Day19 = runPart Advent.Day19.part1

runPart2 :: Day -> Bool -> FilePath -> IO Int
runPart2 Day01 = runPart Advent.Day01.part2
runPart2 Day02 = runPart Advent.Day02.part2
runPart2 Day03 = runPart Advent.Day03.part2
runPart2 Day04 = runPart Advent.Day04.part2
runPart2 Day05 = runPart Advent.Day05.part2
runPart2 Day06 = runPart Advent.Day06.part2
runPart2 Day07 = runPart Advent.Day07.part2
runPart2 Day08 = runPart Advent.Day08.part2
runPart2 Day09 = runPart Advent.Day09.part2
runPart2 Day10 = runPart Advent.Day10.part2
runPart2 Day11 = runPart Advent.Day11.part2
runPart2 Day12 = runPart Advent.Day12.part2
runPart2 Day13 = runPart Advent.Day13.part2
runPart2 Day14 = runPart Advent.Day14.part2
runPart2 Day15 = runPart Advent.Day15.part2
runPart2 Day16 = runPart Advent.Day16.part2
runPart2 Day17 = runPart Advent.Day17.part2
runPart2 Day18 = runPart Advent.Day18.part2
runPart2 Day19 = runPart Advent.Day19.part2
