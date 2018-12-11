{-# LANGUAGE OverloadedStrings #-}

module P11 where

import Util
import Data.List
import qualified Data.Set as S
import System.Environment

p11 :: IO ()
p11 = do
  serial <- getArgs ||> head ||> read
  let candidates = getCandidates 3
      faith = serial + 0
      -- levels = map (\p -> (p, regionPowers serial [1..3] p)) candidates
      -- p1 = sndMax levels
  -- putStrLn $ "p1: " <> show p1
  putStrLn "in progress"

getCandidates i = [(x,y) | x <- [1..300 - (i - 1)], y <- [1..300 - (i - 1)]]

sndMax :: Ord b => [(a,b)] -> (a,b)
sndMax ls = maximumBy (\a b -> compare (snd a) (snd b)) ls

getEdge :: Point -> Int -> S.Set Point
getEdge (tlx,tly) size =
  let s = size - 1
      rightEdge = S.fromList [(tlx + s,y) | y <- [tly..tly + s]]
      bottomEdge = S.fromList [(x,tly + s) | x <- [tlx..tlx + s]]
      corner = S.singleton (tlx + s, tly + s)
  in foldl S.union S.empty [rightEdge, bottomEdge, corner]

{-
regionPowers :: Int -> [Int] -> Point -> [(Int, Int)]
regionPowers serial sizes corner =
  let folder = (\soFar points -> soFar + (regionPower serial points))
  in foldl folder (map (getEdge corner) sizes)
     |> sndMax
-}

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

