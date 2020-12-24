{-# LANGUAGE TupleSections #-}

module Advent.Day24 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    errorBundlePretty,
    many,
    parse,
  )
import Text.Megaparsec.Char (string)

part1 :: Bool -> String -> Int
part1 _ = countTiles . parseInput

part2 :: Bool -> String -> Int
part2 _ = countTiles . update . parseInput
  where
    update = foldr (.) id $ replicate 100 updateFloor

-- Types
newtype Floor = Floor (Map (Int, Int) Bool) deriving (Show)

newtype Adjacency = Adjacency (Map (Int, Int) Int) deriving (Show)

data Direction = NE | NW | SE | SW | E | W deriving (Show)

-- Parsing
type Parser = Parsec Void String

pDirection :: Parser Direction
pDirection =
  choice
    [ NE <$ string "ne",
      NW <$ string "nw",
      SE <$ string "se",
      SW <$ string "sw",
      E <$ string "e",
      W <$ string "w"
    ]

pPath :: Parser [Direction]
pPath = many pDirection

parsePath :: String -> [Direction]
parsePath text = case parse pPath "" text of
  Left bundle -> error (errorBundlePretty bundle)
  Right x -> x

parseInput :: String -> Floor
parseInput = getFloor . map (getCoords . parsePath) . lines

-- Convert a path to a coordinate
getCoords :: [Direction] -> (Int, Int)
getCoords = foldl step (0, 0)

step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) NE = (x + 1, y + 1)
step (x, y) NW = (x, y + 1)
step (x, y) SE = (x, y - 1)
step (x, y) SW = (x - 1, y - 1)
step (x, y) E = (x + 1, y)
step (x, y) W = (x - 1, y)

-- Get a floor from a list of coordinates
getFloor :: [(Int, Int)] -> Floor
getFloor = foldl doFloor (Floor M.empty)

doFloor :: Floor -> (Int, Int) -> Floor
doFloor (Floor f) coords = Floor (M.alter flipTile coords f)

flipTile :: Maybe Bool -> Maybe Bool
flipTile Nothing = Just True
flipTile (Just x) = Just (not x)

-- Count the black tiles
countTiles :: Floor -> Int
countTiles (Floor f) = sum $ map fromEnum (M.elems f)

-- Transpose graph
addNeighbor :: Maybe Int -> Maybe Int
addNeighbor Nothing = Just 1
addNeighbor (Just x) = Just (x + 1)

findNeighbors :: Adjacency -> (Int, Int) -> Bool -> Adjacency
findNeighbors (Adjacency a) x True =
  let n = M.fromList $ (,1) <$> map (step x) [NE, NW, SE, SW, E, W]
   in Adjacency (M.unionWith (+) a n)
findNeighbors a _ False = a

adjInit :: Floor -> Adjacency
adjInit (Floor f) = Adjacency (M.map (fromEnum . not) $ M.filter id f)

countNeighbors :: Floor -> Adjacency
countNeighbors (Floor f) = M.foldlWithKey' findNeighbors (adjInit (Floor f)) f

applyRule :: Floor -> (Int, Int) -> Int -> Bool
applyRule (Floor f) coords n =
  let isBlack = M.findWithDefault False coords f
   in (isBlack && (n == 1)) || (n == 2)

updateFloor :: Floor -> Floor
updateFloor f =
  let (Adjacency a) = countNeighbors f
      f' = M.filter id $ M.mapWithKey (applyRule f) a
   in Floor f'
