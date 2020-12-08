module Advent.Day07 where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

root :: (String, Int)
root = ("shiny gold", 1)

part1 :: String -> Int
part1 text =
  let tree = transposeTree . Map.fromList $ parseData text
   in Set.size (findParentsFrom tree root) - 1

part2 :: String -> Int
part2 text =
  let tree = Map.fromList $ parseData text
   in countBags tree root - 1

--
-- ALGORITHMS
--
findParentsFrom :: TreeMap -> (String, Int) -> Set.Set String
findParentsFrom tree (name, _) =
  let parents = Map.findWithDefault [] name tree
   in foldl (\acc n -> Set.union acc $ findParentsFrom tree n) (Set.singleton name) parents

countBags :: TreeMap -> (String, Int) -> Int
countBags tree (name, dist) =
  let children = Map.findWithDefault [] name tree
      numIn = foldl (\acc n -> acc + countBags tree n) 1 children
   in dist * numIn

--
-- DATA STRUCTURE
--
type TreeMap = Map.Map String [(String, Int)]

-- Transpose a single rule into a list of singleton rules
transposeRule :: (String, [(String, Int)]) -> [(String, [(String, Int)])]
transposeRule (from, elems) = [(to, [(from, weight)]) | (to, weight) <- elems]

-- Transpose a tree by flipping the edges
transposeTree :: TreeMap -> TreeMap
transposeTree tree = Map.fromListWith (++) $ concatMap transposeRule (Map.assocs tree)

--
-- PARSING AND NORMALIZING
--
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
