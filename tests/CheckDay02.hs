module CheckDay02 (checkPart1, checkPart2) where

import Advent (runPart)
import Advent.Day02 (part1, part2)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

checkPart1 :: TestTree
checkPart1 =
  testCase "Part 1" $ do
    res <- runPart "tests/inputs/day02" part1
    assertEqual "Part 1: invalid test output" 2 res

checkPart2 :: TestTree
checkPart2 =
  testCase "Part 2" $ do
    res <- runPart "tests/inputs/day02" part2
    assertEqual "Part 2: invalid test output" 1 res
