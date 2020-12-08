module Advent.Day07 (part1, part2) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, string)

root :: (String, Int)
root = ("shiny gold", 1)

part1 :: String -> Int
part1 text = Set.size (findParents (loadTree text) root) - 1

part2 :: String -> Int
part2 text = countBags (loadTree text) root - 1

-- Algorithms
findParents :: TreeMap -> (String, Int) -> Set.Set String
findParents = foldTree (\(name, _) elems -> Set.unions (Set.singleton name : elems)) . transposeTree

countBags :: TreeMap -> (String, Int) -> Int
countBags = foldTree (\(_, dist) elems -> dist * (1 + sum elems))

-- Tree data structure
type TreeMap = Map.Map String [(String, Int)]

loadTree :: String -> TreeMap
loadTree = Map.fromList . parseData

-- Transpose a single rule into a list of singleton rules
transposeRule :: (String, [(String, Int)]) -> [(String, [(String, Int)])]
transposeRule (from, elems) = [(to, [(from, weight)]) | (to, weight) <- elems]

-- Transpose a tree by flipping the edges
transposeTree :: TreeMap -> TreeMap
transposeTree tree = Map.fromListWith (++) $ concatMap transposeRule (Map.assocs tree)

-- Magic to accumulate a function across the tree
foldTree :: ((String, Int) -> [a] -> a) -> TreeMap -> (String, Int) -> a
foldTree func tree value@(name, _) =
  func value $ map (foldTree func tree) $ Map.findWithDefault [] name tree

-- Parsing
type Parser = Parsec Void String

pEmpty :: Parser [(String, Int)]
pEmpty = do
  _ <- string "no other bags"
  return []

pChild :: Parser (String, Int)
pChild = do
  number <- some digitChar
  _ <- char ' '
  color <- manyTill anySingle (string " bag")
  _ <- optional $ char 's'
  return (color, read number)

pChildren :: Parser [(String, Int)]
pChildren = pChild `sepBy` string ", "

pRule :: Parser (String, [(String, Int)])
pRule = do
  color <- manyTill anySingle (string " bags contain ")
  children <- choice [pEmpty, pChildren]
  _ <- char '.'
  return (color, children)

parseRule :: String -> (String, [(String, Int)])
parseRule text = case parse pRule "" text of
  Left bundle -> error (errorBundlePretty bundle)
  Right x -> x

parseData :: String -> [(String, [(String, Int)])]
parseData = map parseRule . lines
