import Advent (Part (..), runPart)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain tests

main :: IO ()
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testPart Day01a "day01a" "3",
      testPart Day01b "day01b" "5",
      testPart Day02a "day02" "101",
      testPart Day02b "day02" "48",
      testPart Day03a "day03" "4",
      testPart Day03b "day03" "3",
      testPart Day04a "day04" "609043",
      testPart Day05a "day05a" "2",
      testPart Day05b "day05b" "2"
    ]

testPart :: Part -> String -> String -> TestTree
testPart part filename expected = testCase (show part) $ do
  res <- runPart part ("tests/inputs/" ++ filename)
  assertEqual "Invalid test output" expected res
