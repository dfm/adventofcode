module Advent.Day23 (part1, part2) where

import Control.Monad.ST.Strict (runST)
import Data.Foldable (forM_)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

part1 :: Bool -> String -> String
part1 _ = concatMap show . drop 1 . take 9 . runPart [] 100

part2 :: Bool -> String -> Int
part2 _ = product . take 2 . drop 1 . runPart [10 .. 1000000] 10000000

runPart :: [Int] -> Int -> String -> [Int]
runPart extras turns = toList 1 . snd . takeTurns turns . fromList . (++ extras) . parseInput

-- Input parsing
strip :: String -> String
strip = reverse . dropWhile (== '\n') . reverse

parseInput :: String -> [Int]
parseInput = map (\c -> read [c]) . strip

-- Convert lists and linked lists
toList :: Int -> Vector Int -> [Int]
toList n v =
  let m = (n - 1) `mod` V.length v
   in n : toList (v V.! m) v

fromList :: [Int] -> (Int, Vector Int)
fromList [] = (0, V.empty)
fromList xs@(x : _) = runST $ do
  let size = length xs
  mv <- V.unsafeThaw (V.generate size (+ 1))
  forM_ (zip xs (drop 1 (cycle xs))) $ \(x1, x2) -> do
    MV.unsafeWrite mv (x1 - 1) x2
  v <- V.unsafeFreeze mv
  return (x, v)

-- Gameplay
takeTurns :: Int -> (Int, Vector Int) -> (Int, Vector Int)
takeTurns iters (x, v) = runST $ do
  mv <- V.unsafeThaw v

  acc <- newSTRef x
  forM_ [1 .. iters] $ \_ -> do
    -- Get the current cup
    current <- readSTRef acc

    -- Pick up the 3 next ones
    a <- MV.unsafeRead mv (current - 1)
    b <- MV.unsafeRead mv (a - 1)
    c <- MV.unsafeRead mv (b - 1)

    -- Save the 4th for later (it's the next current)
    d <- MV.unsafeRead mv (c - 1)
    writeSTRef acc d

    -- Find the destination cup
    let size = MV.length mv
        offsets = map (\n -> (current - n - 1) `mod` size + 1) [1 ..]
        dest = head $ filter (\n -> n /= a && n /= b && n /= c) offsets

    -- Update all the pointers
    dest' <- MV.unsafeRead mv (dest - 1)
    MV.unsafeWrite mv (current - 1) d
    MV.unsafeWrite mv (c - 1) dest'
    MV.unsafeWrite mv (dest - 1) a

  next <- readSTRef acc
  res <- V.unsafeFreeze mv
  return (next, res)
