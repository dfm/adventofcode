import qualified CheckDay01
import qualified CheckDay02
import qualified CheckDay03
import qualified CheckDay04
import Test.Tasty (TestTree, defaultMain, testGroup)

main = defaultMain tests

main :: IO ()
tests :: TestTree
tests = testGroup "Tests" [day01, day02, day03, day04]

day01 :: TestTree
day01 =
  testGroup
    "Day 1"
    [CheckDay01.checkPart1, CheckDay01.checkPart2]

day02 :: TestTree
day02 =
  testGroup
    "Day 2"
    [CheckDay02.checkPart1, CheckDay02.checkPart2]

day03 :: TestTree
day03 =
  testGroup
    "Day 3"
    [CheckDay03.checkPart1, CheckDay03.checkPart2]

day04 :: TestTree
day04 =
  testGroup
    "Day 4"
    [CheckDay04.checkPart1, CheckDay04.checkPart2]