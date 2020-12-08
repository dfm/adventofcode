module Advent (runPart1, runPart2, getFileName, loadData, Day (..)) where

import qualified Advent.Day01
import qualified Advent.Day02
import qualified Advent.Day03
import qualified Advent.Day04
import qualified Advent.Day05
import qualified Advent.Day06
import qualified Advent.Day07
import qualified Advent.Day08
import qualified Data.ByteString as S
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

loadData :: FilePath -> IO String
loadData filename = do
  contents <- S.readFile filename
  let text = T.unpack $ E.decodeUtf8 contents
  return text

runPart :: (String -> a) -> FilePath -> IO a
runPart func filename = do
  text <- loadData filename
  return $ func text

data Day
  = Day01
  | Day02
  | Day03
  | Day04
  | Day05
  | Day06
  | Day07
  | Day08
  deriving (Enum, Bounded, Show, Read)

getFileName :: Day -> FilePath -> FilePath
getFileName day = (++ "/" ++ map toLower (show day))

runPart1 :: Day -> FilePath -> IO Int
runPart1 Day01 = runPart Advent.Day01.part1
runPart1 Day02 = runPart Advent.Day02.part1
runPart1 Day03 = runPart Advent.Day03.part1
runPart1 Day04 = runPart Advent.Day04.part1
runPart1 Day05 = runPart Advent.Day05.part1
runPart1 Day06 = runPart Advent.Day06.part1
runPart1 Day07 = runPart Advent.Day07.part1
runPart1 Day08 = runPart Advent.Day08.part1

runPart2 :: Day -> FilePath -> IO Int
runPart2 Day01 = runPart Advent.Day01.part2
runPart2 Day02 = runPart Advent.Day02.part2
runPart2 Day03 = runPart Advent.Day03.part2
runPart2 Day04 = runPart Advent.Day04.part2
runPart2 Day05 = runPart Advent.Day05.part2
runPart2 Day06 = runPart Advent.Day06.part2
runPart2 Day07 = runPart Advent.Day07.part2
runPart2 Day08 = runPart Advent.Day08.part2
