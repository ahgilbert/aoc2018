{-# LANGUAGE OverloadedStrings #-}

module P10 where

import Util
import Data.Array as A
import Text.Megaparsec
import Text.Megaparsec.Char

data Star = Star { pos :: Point, vel :: Point }
  deriving (Show)

p10 :: IO ()
p10 = do
  stars <- slurpLinesWith parseSkylight 10
  let idx = idxOfFirstIncrease $ map (boundarySize . (boundaryAt stars)) [0..]
  mapM_ putStrLn $ printStars idx stars
  print idx

idxOfFirstIncrease :: [Int] -> Int
idxOfFirstIncrease [] = 0
idxOfFirstIncrease as =
  zip as (tail as)
  |> map (\(a,b) -> a - b)
  |> takeWhile (> 0)
  |> length

boundarySize :: (Point, Point) -> Int
boundarySize ((xmin,ymin),(xmax,ymax)) =
  (xmax - xmin) * (ymax - ymin)

boundaryAt :: [Star] -> Int -> (Point, Point)
boundaryAt stars t =
  starsAt stars t
  |> boundary

boundary :: [Point] -> (Point, Point)
boundary ps =
  let xs = map fst ps
      ys = map snd ps
      xmin = minimum xs
      xmax = maximum xs
      ymin = minimum ys
      ymax = maximum ys
  in ((xmin,ymin),(xmax,ymax))

printStars :: Int -> [Star] -> [String]
printStars t stars =
  let xs = [xmin..xmax]
      ys = [ymin..ymax]
      allPoints = [(x,y) | x <- xs, y <- ys]
      starLocs = map (starAt t) stars
      bounds@((xmin,ymin),(xmax,ymax)) = boundary starLocs
      arrSeed = zip starLocs $ repeat '#'
      arr = A.accumArray (flip const) ' ' bounds arrSeed
      rows =
        map (\y ->
                let row = [(x,y) | x <- xs]
                in map (arr !) row)
            ys
  in rows

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n as =
  take n as : (chunks n $ drop n as)

starsAt :: [Star] -> Int -> [Point]
starsAt stars t = map (starAt t) stars

starAt :: Int -> Star -> Point
starAt t star =
  let (x,y) = pos star
      (dx,dy) = vel star
  in (x + (dx * t), y + (dy * t))

------------ parsers ---------------
parseSkylight :: Parser Star
parseSkylight = do
  string "position=<"
  space
  xpos <- parseInt
  char ','
  space
  ypos <- parseInt
  string "> velocity=<"
  space
  dx <- parseInt
  char ','
  space
  dy <- parseInt
  char '>'
  return $ Star { pos = (xpos,ypos), vel = (dx, dy) }
