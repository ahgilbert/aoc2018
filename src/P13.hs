{-# LANGUAGE OverloadedStrings #-}

module P13 where

import Util
import qualified Data.Array as A
import Data.List
import Data.Maybe

data Turn = Louie | Onward | Righty
  deriving (Show)
data Direction = N | S | E | W
  deriving (Show)
data TrackSegment = Straight Direction | Intersection | Bend | BackBend | Empty
  deriving (Show)
type Track = A.Array Point TrackSegment
data Cart = Cart { loc :: Point, dir :: Direction, turns :: [Turn] }
instance Show Cart where
  show c = (show $ loc c) <> " " <> (show $ dir c) <> " " <> (show $ head $ turns c)

p13 :: IO ()
p13 = do
  input <- map (map readGrid) <$> lines <$> slurp 13
  let bounds = (length (head input), length input)
      input' = input
               |> zip [0..]
               |> map (\(row, cols) -> zip [(col,row) | col <- [0..]] cols)
      track = map (fmap fst) input'
              |> A.listArray bounds
      carts = input'
              |> concat
              |> filter (\(_,(_,cart)) -> isJust cart)
              |> map (\(coord,(_,cart)) -> (fromJust cart) { loc = coord })
  mapM_ print (sortCarts carts)


sortCarts :: [Cart] -> [Cart]
sortCarts cs =
  let sorter = (\c1 c2 ->
                 let (x1,y1) = loc c1
                     (x2,y2) = loc c2
                     xComp = compare x1 x2
                     yComp = compare y1 y2
                 in if xComp == EQ then yComp else xComp)
  in sortBy sorter cs

readGrid :: Char -> (TrackSegment, Maybe Cart)
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
