module Advent.Day07 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

part1 :: String -> Int
part1 text =
  let tree = flipTree . Map.fromList $ parseData text
   in (Set.size . Set.fromList $ findParentsFrom tree "shiny gold") - 1

part2 :: String -> Int
part2 text =
  let tree = Map.fromList $ parseData text
   in countBags tree ("shiny gold", 1) - 1

-- Solution algorithms
findParentsFrom :: TreeMap -> String -> [String]
findParentsFrom tree name =
  let parents = Map.findWithDefault [] name tree
   in foldl (\x n -> x ++ findParentsFrom tree (fst n)) [name] parents

countBags :: TreeMap -> (String, Int) -> Int
countBags tree (name, dist) =
  let numIn = sum . map (countBags tree) $ Map.findWithDefault [] name tree
   in dist * (1 + numIn)

-- Tree data structure
type TreeMap = Map.Map String [(String, Int)]

flipTree :: TreeMap -> TreeMap
flipTree = Map.foldrWithKey (\from elems newTree -> foldl (addToTree from) newTree elems) Map.empty

addToTree :: String -> TreeMap -> (String, Int) -> TreeMap
addToTree from tree (to, weight) = Map.insertWith (++) to [(from, weight)] tree

-- Parsing and normalizing the data
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
