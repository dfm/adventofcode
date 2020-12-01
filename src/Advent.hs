module Advent (loadListOfInts) where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

parseInts :: String -> [Integer]
parseInts = parseInt . lines

parseInt :: [String] -> [Integer]
parseInt = map read

loadListOfInts :: FilePath -> IO [Integer]
loadListOfInts filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ parseInts contents
