module Main where

import Advent (Part, getFileName, runPart)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO [()]
main = do
  args <- getArgs
  if null args
    then mapM runDay (Just <$> [minBound .. maxBound])
    else mapM (runDay . readMaybe) args

runDay :: Maybe Part -> IO ()
runDay (Just part) = do
  let fn = getFileName part "inputs"
  putStrLn $ "Running: " ++ show part
  result <- runPart part fn
  putStrLn result
runDay Nothing = do
  putStrLn "Unrecognized part"
