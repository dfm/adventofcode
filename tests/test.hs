import qualified CheckDay01
import Test.Tasty

main = defaultMain tests

main :: IO ()
tests :: TestTree
tests = testGroup "Tests" [day01]

day01 :: TestTree
day01 =
  testGroup
    "Day 1"
    [CheckDay01.checkPart1, CheckDay01.checkPart2]