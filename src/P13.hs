{-# LANGUAGE OverloadedStrings #-}

module P13 where

import Util
import Data.List
import Data.Maybe

data Turn = Louie | Onward | Righty
  deriving (Show)
data Direction = N | S | E | W
  deriving (Show)
data Track = Straight Direction | Intersection | Bend | BackBend | Empty
  deriving (Show)
data Cart = Cart { loc :: Point, dir :: Direction, turns :: [Turn] }
instance Show Cart where
  show c = (show $ loc c) <> " " <> (show $ dir c) <> " " <> (show $ head $ turns c)

p13 :: IO ()
p13 = do
  input <- map (map readGrid) <$> lines <$> slurp 1013
  let tracks = map (map fst) input
      carts = zip [0..] input
              |> map (\(row, cols) -> zip [(col,row) | col <- [0..]] cols)
              |> concat
              |> filter (\(_,(_,cart)) -> isJust cart)
              |> map (\(coord,(_,cart)) -> (fromJust cart) { loc = coord })
  mapM_ print carts

readGrid :: Char -> (Track, Maybe Cart)
readGrid '-' = (Straight E, Nothing)
readGrid '<' = (Straight E, Just(newCart W))
readGrid '>' = (Straight E, Just(newCart E))
readGrid '|' = (Straight N, Nothing)
readGrid 'v' = (Straight N, Just(newCart S))
readGrid '^' = (Straight N, Just(newCart N))
readGrid '+' = (Intersection, Nothing)
readGrid '/' = (Bend, Nothing)
readGrid '\\' = (BackBend, Nothing)
readGrid ' ' = (Empty, Nothing)
readGrid _ = undefined

newCart d = Cart { loc = (0,0), dir = d, turns = cycle [Louie, Onward, Righty] }
