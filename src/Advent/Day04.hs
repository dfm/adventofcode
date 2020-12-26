{-# LANGUAGE OverloadedStrings #-}

module Advent.Day04 where

import Advent.Solver (Solver (..))
import Crypto.Hash (Context, MD5 (..))
import qualified Crypto.Hash as Hash
import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

day04a :: Solver (Context MD5) Int
day04a =
  Solver
    { sParse = Just . getBaseContext,
      sSolve = Just . findFirst "00000",
      sShow = show
    }

day04b :: Solver (Context MD5) Int
day04b =
  Solver
    { sParse = Just . getBaseContext,
      sSolve = Just . findFirst "000000",
      sShow = show
    }

toByteString :: String -> ByteString
toByteString = encodeUtf8 . pack

getBaseContext :: String -> Context MD5
getBaseContext = Hash.hashUpdate (Hash.hashInitWith MD5) . toByteString

tryNumber :: Context MD5 -> Int -> String
tryNumber ctx = show . Hash.hashFinalize . Hash.hashUpdate ctx . toByteString . show

findFirst :: String -> Context MD5 -> Int
findFirst prefix ctx = head $ filter (isPrefixOf prefix . tryNumber ctx) [0 ..]
