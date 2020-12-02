import qualified CheckDay01
import qualified CheckDay02
import Test.Tasty

main = defaultMain tests

main :: IO ()
tests :: TestTree
tests = testGroup "Tests" [day01, day02]

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