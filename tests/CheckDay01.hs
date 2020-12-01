module CheckDay01 (checkPart1, checkPart2) where

import Advent.Day01
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

readDay1 :: IO [Integer]
readDay1 = do
  handle <- openFile "tests/inputs/day01.txt" ReadMode
  contents <- hGetContents handle
  return $ parseInts contents

checkPart1 :: TestTree
checkPart1 =
  testCase "Part 1" $ do
    numbers <- readDay1
    assertEqual "Part 1: invalid test output" 514579 (part1 numbers)

checkPart2 :: TestTree
checkPart2 =
  testCase "Part 2" $ do
    numbers <- readDay1
    assertEqual "Part 2: invalid test output" 241861950 (part2 numbers)
