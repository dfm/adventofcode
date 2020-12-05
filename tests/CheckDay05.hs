module CheckDay05 (checkPart1) where

import Advent (runPart)
import Advent.Day05 (part1)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

checkPart1 :: TestTree
checkPart1 =
  testCase "Part 1" $ do
    res <- runPart "tests/inputs/day05" part1
    assertEqual "Part 1: invalid test output" 820 res
