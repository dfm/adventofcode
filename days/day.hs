module Main where

import Advent (Day, getFileName, runPart1, runPart2)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO [()]
main = do
  args <- getArgs
  if null args
    then mapM runDay (Just <$> [minBound .. maxBound])
    else mapM (runDay . readMaybe) args

runDay :: Maybe Day -> IO ()
runDay (Just day) = do
  let fn = getFileName day "inputs"
  putStrLn $ "Running: " ++ show day
  result1 <- runPart1 day False fn
  print result1
  result2 <- runPart2 day False fn
  print result2
runDay Nothing = do
  putStrLn "Unrecognized day"
