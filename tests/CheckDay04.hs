module CheckDay04 (checkPart1, checkPart2) where

import Advent (runPart)
import Advent.Day04 (part1, part2)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

checkPart1 :: TestTree
checkPart1 =
  testCase "Part 1" $ do
    res <- runPart "tests/inputs/day04" part1
    assertEqual "Part 1: invalid test output" 2 res

checkPart2 :: TestTree
checkPart2 =
  testCase "Part 2" $ do
    res <- runPart "tests/inputs/day04" part2
    assertEqual "Part 2: invalid test output" 2 res
