{-# LANGUAGE OverloadedStrings #-}

module Advent.Day18 (part1, part2) where

import Data.List.Split (splitOn)
import Data.Text (pack, replace, unpack)
import Text.Read (readMaybe)

part1 :: Bool -> String -> Int
part1 _ = sum . map (evaluate precedence1) . lines

part2 :: Bool -> String -> Int
part2 _ = sum . map (evaluate precedence2) . lines

-- Operator precedence
precedence1 :: Token -> Int
precedence1 (Operator "(") = -1
precedence1 _ = 0

precedence2 :: Token -> Int
precedence2 (Operator "+") = 1
precedence2 (Operator "(") = -1
precedence2 _ = 0

-- Parsing
data Token = Number String | Operator String deriving (Show)

data Parser = Parser [Token] [Token] deriving (Show)

newParser :: Parser
newParser = Parser [] []

fromString :: String -> Token
fromString text = case readMaybeInt text of
  Just _ -> Number text
  Nothing -> Operator text
  where
    readMaybeInt :: String -> Maybe Int
    readMaybeInt = readMaybe

tokenize :: String -> [Token]
tokenize = map fromString . splitOn " " . unpack . replace ")" " )" . replace "(" "( " . pack

-- Shuntyard algorithm
parseToken :: (Token -> Int) -> Parser -> Token -> Parser
parseToken _ (Parser stack ops) token@(Number _) = Parser (token : stack) ops
parseToken _ (Parser stack ops) token@(Operator "(") = Parser stack (token : ops)
parseToken _ parser (Operator ")") = evalSubExpr parser
parseToken precedence parser token =
  let (Parser stack ops) = popOps precedence (precedence token) parser
   in Parser stack (token : ops)

-- Reverse Polish
applyOp :: [Token] -> Token -> [Token]
applyOp ((Number x) : (Number y) : stack) (Operator op)
  | op == "+" = Number (show (b + a)) : stack
  | op == "*" = Number (show (b * a)) : stack
  | otherwise = error "Unknown operator"
  where
    readInt :: String -> Int
    readInt = read
    a = readInt x
    b = readInt y
applyOp _ _ = error "Syntax error"

evalSubExpr :: Parser -> Parser
evalSubExpr (Parser stack (Operator "(" : ops)) = Parser stack ops
evalSubExpr (Parser stack (o : ops)) = evalSubExpr $ Parser (applyOp stack o) ops
evalSubExpr (Parser stack []) = Parser stack []

popOps :: (Token -> Int) -> Int -> Parser -> Parser
popOps precedence ord (Parser stack (o : ops))
  | precedence o >= ord = popOps precedence ord $ Parser (applyOp stack o) ops
  | otherwise = Parser stack (o : ops)
popOps _ _ (Parser stack []) = Parser stack []

evaluate :: (Token -> Int) -> String -> Int
evaluate precedence text =
  let (Parser (Number x : _) _) = popOps precedence 0 $ foldl (parseToken precedence) newParser (tokenize text)
   in read x
