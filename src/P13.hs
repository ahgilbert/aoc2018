{-# LANGUAGE OverloadedStrings #-}

module P13 where

import Util
import qualified Data.Array as A
import Data.Array ((!))
import Data.List
import Data.Maybe

data Turn = Louie | Onward | Righty
  deriving (Show, Eq)
data Direction = N | S | E | W
  deriving (Show, Eq)
data TrackSegment = Straight | Intersection | Bend | BackBend | Empty
  deriving (Show, Eq)
type Track = A.Array Point TrackSegment
data Cart = Cart { loc :: Point, dir :: Direction, turns :: [Turn] }
  deriving (Eq)
instance Show Cart where
  show c = (show $ loc c) <> " " <> (show $ dir c) <> " " <> (show $ head $ turns c)

p13 :: IO ()
p13 = do
  input <- map (map readGrid) <$> lines <$> slurp 13
  let bounds = ((0,0),(length (head input), length input))
      input' = input
               |> zip [0..]
               |> map (\(row, cols) -> zip [(col,row) | col <- [0..]] cols)
      track = (map . map) (fmap fst) input'
              |> concat
              |> A.accumArray (flip const) Empty bounds
      carts = input'
              |> concat
              |> filter (\(_,(_,cart)) -> isJust cart)
              |> map (\(coord,(_,cart)) -> (fromJust cart) { loc = coord })
      faith = iterate (step track) carts
      crash = dropWhile (not . hasCrash) faith
              |> head
              |> detectCrash
  print crash

hasCrash :: [Cart] -> Bool
hasCrash cs = detectCrash cs |> length |> (> 0)

detectCrash :: [Cart] -> [Point]
detectCrash carts =
  carts
  |> map loc
  |> sort
  |> group
  |> filter (\gs -> length gs > 1)
  |> map head

step :: Track -> [Cart] -> [Cart]
step track carts =
  map go carts
  where
    go c =
      let next = move (loc c) (dir c)
          section = track ! next
          newDir = case section of
                     Bend -> Louie
                     BackBend -> Righty
                     Intersection -> head (turns c)
                     Straight -> Onward
                     Empty -> undefined
                   |> turn (dir c)
          turns' = if (section == Intersection) then tail (turns c) else turns c
      in c { loc = next, dir = newDir, turns = turns' }

move (x,y) N = (x,y - 1)
move (x,y) S = (x,y + 1)
move (x,y) E = (x + 1,y)
move (x,y) W = (x - 1,y)

turn N Louie = W
turn N Righty = E
turn S Louie = E
turn S Righty = W
turn E Louie = N
turn E Righty = S
turn W Louie = S
turn W Righty = N
turn d Onward = d


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
readGrid '-' = (Straight, Nothing)
readGrid '<' = (Straight, Just(newCart W))
readGrid '>' = (Straight, Just(newCart E))
readGrid '|' = (Straight, Nothing)
readGrid 'v' = (Straight, Just(newCart S))
readGrid '^' = (Straight, Just(newCart N))
readGrid '+' = (Intersection, Nothing)
readGrid '/' = (Bend, Nothing)
readGrid '\\' = (BackBend, Nothing)
readGrid ' ' = (Empty, Nothing)
readGrid _ = undefined

newCart d = Cart { loc = (0,0), dir = d, turns = cycle [Louie, Onward, Righty] }
