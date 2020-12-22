import Advent
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain tests

main :: IO ()
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ dayTest Day01 "day01" (Just "514579") (Just "241861950"),
      dayTest Day02 "day02" (Just "2") (Just "1"),
      dayTest Day03 "day03" (Just "7") (Just "336"),
      dayTest Day04 "day04" (Just "2") (Just "2"),
      dayTest Day05 "day05" (Just "820") Nothing,
      dayTest Day06 "day06" (Just "11") (Just "6"),
      dayTest Day07 "day07" (Just "4") (Just "32"),
      dayTest Day08 "day08" (Just "5") (Just "8"),
      dayTest Day09 "day09" (Just "127") (Just "62"),
      dayTest Day10 "day10" (Just "220") (Just "19208"),
      dayTest Day11 "day11" (Just "37") (Just "26"),
      dayTest Day12 "day12" (Just "25") (Just "286"),
      dayTest Day13 "day13" (Just "295") (Just "1068781"),
      dayTest Day14 "day14a" (Just "165") Nothing,
      dayTest Day14 "day14b" Nothing (Just "208"),
      dayTest Day15 "day15" (Just "436") Nothing,
      dayTest Day16 "day16a" (Just "71") Nothing,
      dayTest Day16 "day16b" Nothing (Just "13"),
      dayTest Day17 "day17" (Just "112") (Just "848"),
      dayTest Day18 "day18" (Just "26386") (Just "693942"),
      dayTest Day19 "day19a" (Just "2") Nothing,
      dayTest Day19 "day19b" Nothing (Just "12"),
      dayTest Day21 "day21" (Just "5") (Just "\"mxmxvkd,sqjhc,fvjkl\""),
      dayTest Day22 "day22" (Just "306") (Just "291"),
      dayTest Day23 "day23" Nothing Nothing
    ]

dayTest :: Day -> String -> Maybe String -> Maybe String -> TestTree
dayTest day filename ex1 ex2 =
  testGroup
    (show day ++ ": " ++ filename)
    [ part1Test day filename ex1,
      part2Test day filename ex2
    ]

part1Test :: Day -> String -> Maybe String -> TestTree
part1Test day filename (Just expected) = testCase "Part 1" $ do
  res <- runPart1 day True ("tests/inputs/" ++ filename)
  assertEqual "Part 1: invalid test output" expected res
part1Test _ _ Nothing = testCase "Part 1 (N/A)" $ do return ()

part2Test :: Day -> String -> Maybe String -> TestTree
part2Test day filename (Just expected) = testCase "Part 2" $ do
  res <- runPart2 day True ("tests/inputs/" ++ filename)
  assertEqual "Part 1: invalid test output" expected res
part2Test _ _ Nothing = testCase "Part 2 (N/A)" $ do return ()
