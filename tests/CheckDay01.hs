module CheckDay01 (checkPart1, checkPart2) where

import Advent (loadListOfInts)
import Advent.Day01 (part1, part2)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

checkPart1 :: TestTree
checkPart1 =
  testCase "Part 1" $ do
    numbers <- loadListOfInts "tests/inputs/day01.txt"
    assertEqual "Part 1: invalid test output" 514579 (part1 numbers)

checkPart2 :: TestTree
checkPart2 =
  testCase "Part 2" $ do
    numbers <- loadListOfInts "tests/inputs/day01.txt"
    assertEqual "Part 2: invalid test output" 241861950 (part2 numbers)
