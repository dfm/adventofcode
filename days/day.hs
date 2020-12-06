module Main where

import Advent
import System.Environment
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
  result1 <- runPart1 day fn
  print result1
  result2 <- runPart2 day fn
  print result2
runDay Nothing = do
  putStrLn "Unrecognized day"
