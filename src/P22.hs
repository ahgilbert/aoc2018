{-# LANGUAGE OverloadedStrings #-}

module P22 where

import Util
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

-- depth = 8787
-- target = (10,725)

type ModeMap = M.Map Point Int

p22 :: IO ()
p22 = print $ p22' 8727 (10,725)

p22' :: Int -> (Int, Int) -> ModeMap
p22' depth target@(x,y) =
  let northEdge = [(x,0) | x <- [0..y]]
      north = map (\p@(xx,yy) -> (p,(x * 16807) `mod` 20183)) northEdge
      m = M.fromList north
  in m

erosionLevel :: Int -> Point -> ModeMap -> Point -> ModeMap
erosionLevel depth target m p@(x,y)
  | p == target = M.insert p 0 m
  | x == 0 || y == 0 = M.insert p (el depth target p) m
  | True = M.insert p ((grab (x-1,y) * grab (x,y-1)) `mod` 20183) m
  where grab p = fromJust $ M.lookup p m

el :: Int -> Point -> Point -> Int
el d t p@(x,y)
  | p == t = 0
  | x == 0 = (d + y * 48271) `mod` 20183
  | y == 0 = (d + x * 16807) `mod` 20183
  | True   = undefined
