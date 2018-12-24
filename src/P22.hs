{-# LANGUAGE OverloadedStrings #-}

module P22 where

import Util
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

-- depth = 8787
-- target = (10,725)

type ModeMap = M.Map Point Int

p22 :: IO ()
p22 = print $ p22' 8727 (10,725)

p22' :: Int -> (Int, Int) -> ModeMap
p22' depth target@(xx,yy) =
  let northEdge = [(x,0) | x <- [0..xx]]
      north = map (\p@(x,_) -> (p,(x * 16807) `mod` 20183)) northEdge
      m = M.fromList north
  in foldl (folder depth target xx) m [1..yy]

folder :: Int -> Point -> Int -> ModeMap -> Int -> ModeMap
folder d t w m y = foldl (erosionLevel d t) m [(x,y) | x <- [0..w]]

erosionLevel :: Int -> Point -> ModeMap -> Point -> ModeMap
erosionLevel depth target m p@(x,y)
  | p == target = M.insert p 0 m
  | x == 0 || y == 0 = M.insert p (el depth target p) m
  | True = M.insert p ((grab (x-1,y) * grab (x,y-1)) `mod` 20183) m
  where grab p = fromJust $ M.lookup p m

drawCave :: ModeMap -> [String]
drawCave m =
  M.keys m
  |> sortBy (\a b ->
              let ys = compare (snd a) (snd b)
              in if ys == EQ then compare (fst a) (fst b) else ys)
  |> groupBy (\a b -> snd a == snd b)
  |> (fmap . fmap) (\p -> fromJust $ drawSpot <$> (`mod` 3) <$> M.lookup p m)

drawSpot 0 = '.'
drawSpot 1 = '='
drawSpot 2 = '|'
drawSpot _ = 'X'

el :: Int -> Point -> Point -> Int
el d t p@(x,y)
  | p == t = 0
  | x == 0 = (d + y * 48271)
  | y == 0 = (d + x * 16807)
  | True   = undefined
