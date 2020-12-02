module CheckDay02 (checkPart1, checkPart2) where

import Advent.Day02 (part1, part2)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

checkPart1 :: TestTree
checkPart1 =
  testCase "Part 1" $ do
    withFile
      "tests/inputs/day02"
      ReadMode
      ( \handle -> do
          text <- hGetContents handle
          assertEqual "Part 1: invalid test output" (Right 2) (part1 $ lines text)
      )

checkPart2 :: TestTree
checkPart2 =
  testCase "Part 2" $ do
    withFile
      "tests/inputs/day02"
      ReadMode
      ( \handle -> do
          text <- hGetContents handle
          assertEqual "Part 2: invalid test output" (Right 1) (part2 $ lines text)
      )
