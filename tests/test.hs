import Advent
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain tests

main :: IO ()
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ dayTest Day01 (Just 514579) (Just 241861950),
      dayTest Day02 (Just 2) (Just 1),
      dayTest Day03 (Just 7) (Just 336),
      dayTest Day04 (Just 2) (Just 2),
      dayTest Day05 (Just 820) Nothing,
      dayTest Day06 (Just 11) (Just 6),
      dayTest Day07 (Just 4) (Just 32),
      dayTest Day08 (Just 5) (Just 8),
      dayTest Day09 (Just 127) (Just 62),
      dayTest Day10 (Just 220) (Just 19208),
      dayTest Day11 (Just 37) (Just 26),
      dayTest Day12 (Just 25) (Just 286),
      dayTest Day13 Nothing Nothing
    ]

dayTest :: Day -> Maybe Int -> Maybe Int -> TestTree
dayTest day ex1 ex2 = testGroup (show day) [part1Test day ex1, part2Test day ex2]

part1Test :: Day -> Maybe Int -> TestTree
part1Test day (Just expected) = testCase "Part 1" $ do
  res <- runPart1 day True (getFileName day "tests/inputs")
  assertEqual "Part 1: invalid test output" expected res
part1Test _ Nothing = testCase "Part 1 (not implemented)" $ do return ()

part2Test :: Day -> Maybe Int -> TestTree
part2Test day (Just expected) = testCase "Part 2" $ do
  res <- runPart2 day True (getFileName day "tests/inputs")
  assertEqual "Part 1: invalid test output" expected res
part2Test _ Nothing = testCase "Part 2 (not implemented)" $ do return ()
