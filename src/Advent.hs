module Advent (loadListOfInts) where

import System.IO (Handle, hGetContents)

parseInts :: String -> [Integer]
parseInts = parseInt . lines

parseInt :: [String] -> [Integer]
parseInt = map read

loadListOfInts :: Handle -> IO [Integer]
loadListOfInts handle = do
  contents <- hGetContents handle
  return $ parseInts contents
