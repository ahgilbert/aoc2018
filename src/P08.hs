{-# LANGUAGE OverloadedStrings #-}

module P08 where

import Util
import Data.Either
import Data.Tree
import Text.Megaparsec
import Text.Megaparsec.Char

type Memory = Tree [Int]

readInt :: String -> Int
readInt = read

p08 :: IO ()
p08 = do
  input <- map readInt <$> words <$> slurp 8
  let (tree, rest) = buildTree input
  putStrLn $ "Part 1: " <> (show $ sum $ concat $ flatten tree)
  putStrLn $ "Part 2: " <> (show $ scoreTree tree)

buildTree :: [Int] -> (Memory, [Int])
buildTree (nKids:nMeta:rest) =
  let (kids, remainder) = buildForest nKids ([], rest)
      metadata = take nMeta remainder
  in (Node { rootLabel = metadata, subForest = kids }, drop nMeta remainder)

buildForest :: Int -> ([Memory], [Int]) -> ([Memory], [Int])
buildForest 0 rest = rest
buildForest n (kids, rest) =
  let (child, rest') = buildTree rest
  in buildForest (n - 1) ((kids ++ [child]), rest')

scoreTree t =
  if (null (subForest t))
  then ((rootLabel t)) |> sum
  else
    let kids = subForest t
        idxes = rootLabel t
                |> filter (\i -> i > 0 && i <= (length kids))
                |> map (\i -> i - 1)
        candidateKids = map (kids !!) idxes
        scores = map scoreTree candidateKids
    in sum $ map scoreTree (candidateKids)

----------- parsers -------------

parseIntToken :: Parser Int
parseIntToken = do
  i <- parseInt
  space
  return i

parseMemory :: Parser [Int]
parseMemory = many parseIntToken
