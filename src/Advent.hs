module Advent (runPart) where

import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

runPart :: FilePath -> (String -> a) -> IO a
runPart filename func = do
  contents <- S.readFile filename
  let text = T.unpack $ E.decodeUtf8 contents
  return $ func text
