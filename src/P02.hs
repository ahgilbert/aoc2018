{-# LANGUAGE OverloadedStrings #-}

module P02 where

import Util
import Data.List

p02_1 :: IO ()
p02_1 = do
  file <- map sort <$> lines <$> slurp 2
  let twos = filter (hasNlet 2) file
  let threes = filter (hasNlet 3) file
  print (length twos)
  print (length threes)
  print (length twos * length threes)

p02_2 :: IO ()
p02_2 = do
  pairs <- allPairs <$> lines <$> slurp 2
  let origLen = length $ fst $ head pairs
  print origLen
  let faith = map reportMatches pairs
  let hope = filter (\ms -> length ms == (origLen - 1)) faith
  print hope

hasNlet :: (Eq a) => Int -> [a] -> Bool
hasNlet _ [] = False
hasNlet n cs@(c:_) =
  let faith = takeWhile (== c) cs
  in if (length faith) == n
     then True
     else hasNlet n (dropWhile (== c) cs)

allPairs :: [a] -> [(a,a)]
allPairs xs = [(a,b) | a <- xs, b <- xs]

reportMatches :: (Eq a) => ([a],[a]) -> [a]
reportMatches (as,bs) =
  map snd $ filter fst $ zipWith (\a b -> (a == b, a)) as bs
