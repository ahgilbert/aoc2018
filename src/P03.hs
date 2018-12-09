{-# LANGUAGE OverloadedStrings #-}

module P03 where

import Util
import Data.Array
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

data Patch = Patch { ident :: Int, corner :: (Int, Int), size :: (Int, Int) }
  deriving (Show)

p03 = do
  patches <- slurpLinesWith parsePatch 3
  let claims = map getPoints patches
  let arr = accumArray (+) 0 ((0,0),(1000,1000)) (zip (concat (map snd claims)) (repeat 1))
  let overlap = elems arr |> filter (> 1) |> length
  print overlap
  let valsByPatch = map (\p -> (fst p, getVals arr (snd p))) claims
  let patchesWithoutOverlap = filter (\p -> all (== 1) (snd p)) valsByPatch
  print (map fst patchesWithoutOverlap)

getPoints :: Patch -> (Int, [(Int, Int)])
getPoints p =
  (ident p, [(x,y) | x <- map (+ (fst $ corner p)) [0..(fst $ size p) - 1],
                     y <- map (+ (snd $ corner p)) [0..(snd $ size p) - 1]])

getVals arr points = map (arr !) points

-------------- parsers ----------------------
parsePatch :: Parser Patch
parsePatch = do
  id <- parseId
  string " @ "
  corner <- parseCorner
  string ": "
  size <- parseSize
  return $ Patch { ident = id, corner = corner, size = size }

parseUnsignedInt = some digitChar ||> read

parseCorner :: Parser (Int, Int)
parseCorner = do
  x <- parseUnsignedInt
  char ','
  y <- parseUnsignedInt
  return (x,y)

parseSize :: Parser (Int, Int)
parseSize = do
  x <- parseUnsignedInt
  char 'x'
  y <- parseUnsignedInt
  return (x,y)

parseId :: Parser Int
parseId = do
  char '#'
  parseUnsignedInt
