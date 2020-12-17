module Advent.Day17 where

import Control.Monad (replicateM)
import Control.Monad.ST.Strict (runST)
import Data.Bifunctor (first)
import Data.Foldable (forM_)
import Data.List (foldl')
import Data.Traversable (forM)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

part1 :: Bool -> String -> Int
part1 _ = runPart 3 6

part2 :: Bool -> String -> Int
part2 _ = runPart 4 6

-- Runner
runPart :: Int -> Int -> String -> Int
runPart dim iters text =
  let (shape, inp) = parseInput dim iters text
      (size, strides) = getStrides shape
      offsets = getOffsets strides
      inds = getValidIndices shape strides
      initial = ((V.replicate size False) V.// (map (first (ravel strides)) inp))
   in V.length (V.filter id $ doLoops iters offsets inds initial)

doLoops :: Int -> [Int] -> [Int] -> V.Vector Bool -> V.Vector Bool
doLoops iters offsets inds array = foldl (\a _ -> doLoop offsets inds a) array [1 .. iters]

doLoop :: [Int] -> [Int] -> V.Vector Bool -> V.Vector Bool
doLoop offsets inds array = runST $ do
  tmp <- V.thaw array
  forM_ inds $ \i -> do
    x <- forM offsets $ \d -> do
      return $ array V.! (i + d)
    let self = array V.! i
        n = length $ filter id x
        up = (self && ((n == 2) || (n == 3))) || ((n == 3) && not self)
    MV.unsafeWrite tmp i up
  V.freeze tmp

-- Parse the data into an appropriately padded array
parseInput :: Int -> Int -> String -> ([Int], [([Int], Bool)])
parseInput dim iters text =
  let rows = lines text
      delta = iters + 1
      shape = [2 * delta + length rows, 2 * delta + length (head rows)] ++ replicate (dim - 2) (1 + 2 * delta)
      extra = replicate (dim - 2) delta
   in (shape, concat $ zipWith (\i row -> zipWith (\j val -> ([i, j] ++ extra, val == '#')) [delta ..] row) [delta ..] rows)

-- Strides and coords to the flattened index
ravel :: [Int] -> [Int] -> Int
ravel strides = foldl' (\acc (s, i) -> acc + s * i) 0 . zip strides

getOffsets :: [Int] -> [Int]
getOffsets strides =
  let coords = filter (any (/= 0)) $ replicateM (length strides) [-1, 0, 1]
   in map (ravel strides) coords

getValidIndices :: [Int] -> [Int] -> [Int]
getValidIndices shape strides = map (ravel strides) $ sequence [[1 .. n - 2] | n <- shape]

-- Compute the strides from a shape
getStrides :: [Int] -> (Int, [Int])
getStrides [] = (1, [])
getStrides (d : ds) =
  let (next, inner) = getStrides ds
   in (next * d, next : inner)
