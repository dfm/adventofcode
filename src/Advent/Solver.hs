module Advent.Solver where

data Solver a b = Solver
  { sParse :: String -> Maybe a,
    sSolve :: a -> Maybe b,
    sShow :: b -> String
  }
