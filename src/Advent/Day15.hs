module Advent.Day15 (part1, part2) where

import Control.Monad.ST.Strict (runST)
import Data.Foldable (forM_)
import Data.List.Split (splitOn)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as MV

part1 :: Bool -> String -> Int
part1 _ = doLoop 2020 . map read . splitOn ","

part2 :: Bool -> String -> Int
part2 _ = doLoop 30000000 . map read . splitOn ","

doLoop :: Int -> [Int] -> Int
doLoop iters xs = runST $ do
  -- Allocate memory for the workspace
  work <- MV.replicate (max iters (1 + maximum xs)) (-1)

  -- Save the initial numbers to the workspace
  forM_ (zip [0 ..] xs) $ \(i, x) -> do
    MV.write work x i

  -- Do the loop
  number <- newSTRef $ last xs
  forM_ [length xs - 1 .. iters - 2] $ \turn -> do
    num <- readSTRef number
    prev <- MV.unsafeRead work num
    let n = if prev < 0 then 0 else turn - prev
    writeSTRef number n
    MV.unsafeWrite work num turn

  readSTRef number
