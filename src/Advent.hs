module Advent (runPart, getFileName, loadData, Part (..)) where

import qualified Advent.Day01
import qualified Advent.Day02
import qualified Advent.Day03
import qualified Advent.Day04
import Advent.Solver (Solver (..))
import qualified Data.ByteString as S
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data Part
  = Day01a
  | Day01b
  | Day02a
  | Day02b
  | Day03a
  | Day03b
  | Day04a
  | Day04b
  deriving (Enum, Bounded, Show, Read)

loadData :: FilePath -> IO String
loadData filename = do
  contents <- S.readFile filename
  let text = T.strip $ E.decodeUtf8 contents
  return $ T.unpack text

getFileName :: Part -> FilePath -> FilePath
getFileName part = (++ "/" ++ map toLower (init $ show part))

runSolver :: Solver a b -> String -> Maybe String
runSolver s text = sShow s <$> (sSolve s =<< sParse s text)

resolve :: Part -> String -> Maybe String
resolve Day01a = runSolver Advent.Day01.day01a
resolve Day01b = runSolver Advent.Day01.day01b
resolve Day02a = runSolver Advent.Day02.day02a
resolve Day02b = runSolver Advent.Day02.day02b
resolve Day03a = runSolver Advent.Day03.day03a
resolve Day03b = runSolver Advent.Day03.day03b
resolve Day04a = runSolver Advent.Day04.day04a
resolve Day04b = runSolver Advent.Day04.day04b

runPart :: Part -> FilePath -> IO String
runPart part filename = do
  text <- loadData filename
  case resolve part text of
    Nothing -> error $ "Failed to run " ++ show part
    Just x -> return x
