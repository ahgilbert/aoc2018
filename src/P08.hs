{-# LANGUAGE OverloadedStrings #-}

module P08 where

import Util
import Data.Either
import Data.Tree
import Text.Megaparsec
import Text.Megaparsec.Char

type Memory = Tree [Int]

p08 :: IO ()
p08 = do
  input <- lines <$> slurp 8
  let license = rights $ map (runParser parseMemory "") input
  print "write some code"

buildTree :: [Int] -> (Memory, [Int])
buildTree (nKids:nMeta:rest) =
  let (kids, remainder) = buildForest nKids ([], rest)
      metadata = take nMeta remainder
  in (Node { rootLabel = metadata, subForest = kids }, drop nMeta remainder)

buildForest :: Int -> ([Memory], [Int]) -> ([Memory], [Int])
buildForest 0 rest = rest
buildForest n (kids, rest) =
  let (child, rest') = buildTree rest
  in buildForest (n - 1) ((child:kids), rest')

----------- parsers -------------

parseIntToken :: Parser Int
parseIntToken = do
  i <- parseInt
  space
  return i

parseMemory :: Parser [Int]
parseMemory = do
  many parseIntToken
