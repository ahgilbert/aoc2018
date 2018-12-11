{-# LANGUAGE OverloadedStrings #-}

module P11 where

import Util
import Data.List
import qualified Data.Set as S
import System.Environment

p11 :: IO ()
p11 = do
  serial <- getArgs ||> head ||> read
  let p1 = p11_1 serial
  print $ "p1: " <> (show p1)

p11_1 :: Int -> Point
p11_1 serial =
  let candidates = getCandidates 3
      sq3s = candidates
             |> map (\p -> (p, regionPowers serial 3 p)) -- [(size, score)]
             |> (map . fmap) (\rs -> snd $ rs !! 2)
      p1 = sndMax sq3s
           |> fst
  in p1

getCandidates i = [(x,y) | x <- [1..300 - (i - 1)], y <- [1..300 - (i - 1)]]

sndMax :: Ord b => [(a,b)] -> (a,b)
sndMax ls = maximumBy (\a b -> compare (snd a) (snd b)) ls

getEdge :: Point -> Int -> S.Set Point
getEdge _ 0 = S.empty
getEdge (tlx,tly) size =
  let s = size - 1
      rightEdge = S.fromList [(tlx + s,y) | y <- [tly..tly + s]]
      bottomEdge = S.fromList [(x,tly + s) | x <- [tlx..tlx + s]]
      corner = S.singleton (tlx + s, tly + s)
  in foldl S.union S.empty [rightEdge, bottomEdge, corner]

-- Given a max size and a corner, give (size, score) pairs for regions anchored to corner
regionPowers :: Int -> Int -> Point -> [(Int, Int)]
regionPowers serial maxSize corner =
  let edges = map (getEdge corner) [1..maxSize]
      powerSums = map (regionPower serial) edges
                  |> scanl (+) 0
                  |> tail
                  |> zip [1..]
  in powerSums

regionPower :: Int -> S.Set Point -> Int
regionPower serial points =
  S.foldl (\soFar p -> (powerLevel serial p) + soFar) 0 points

powerLevel :: Int -> Point -> Int
powerLevel serial (x,y) =
  let rack = x + 10
  in ((rack * y) + serial) * rack
     |> hundreds
     |> (\i -> i - 5)

hundreds :: Int -> Int -- return hundreds digit
hundreds n =
  (n `mod` 1000) `div` 100

