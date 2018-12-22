{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module P15 where

import Util
import Data.List
import Data.Graph.AStar
import qualified Data.Array.Unboxed as A
import qualified Data.Map.Strict as M

type RogueMap = A.Array Point Char
newtype BPoint = BPoint Point
  deriving (Eq, Show)
instance Ord BPoint where
  compare = readingCompare

p15 :: IO ()
p15 = do
  input <- lines <$> slurp 1015
  let raw = parseRogue input
      cave = fmap (\c -> if c == 'G' || c == 'E' then '.' else c) raw
      goblins = raw
                |> A.assocs
                |> filter (\(_,v) -> v == 'G')
                |> (map . fmap) mkGoblin
                |> M.fromList
      elves = raw
                |> A.assocs
                |> filter (\(_,v) -> v == 'E')
                |> (map . fmap) mkElf
                |> M.fromList
  printRogueMap cave
  putStrLn ""
  mapM_ putStrLn input

------- problem logic ----------

data Creature = Creature { hp :: Int, attack :: Int }
mkCreature = Creature { hp = 200, attack = 3 }

mkGoblin = const mkCreature
mkElf = const mkCreature

getPassability = undefined

----------- utils --------------

-- Consider all shortest paths from start to end. Take their first steps,
-- and give the one that comes first in reading order
getFirstStep :: RogueMap -> Point -> Point -> Point
getFirstStep rm start end =
  undefined
  |> map head
  |> sort
  |> head

printRogueMap :: RogueMap -> IO ()
printRogueMap = (mapM_ putStrLn) . showRogueMap

showRogueMap rm =
    let ((xmin,ymin),(xmax,ymax)) = A.bounds rm
        rows = map (\y -> [(x,y) | x <- [xmin..xmax]]) [ymin..ymax]
    in map (map (\k -> rm A.! k)) rows

readingCompare :: BPoint -> BPoint -> Ordering
readingCompare (BPoint (xa,ya)) (BPoint (xb,yb)) =
  if (compare ya yb) == EQ
  then (compare xa xb)
  else (compare ya yb)
----------- parsers ---------------

parseRogue :: [String] -> RogueMap
parseRogue rs = A.array ((0,0),(h-1,w-1)) assocs
  where
    w      = length (head rs)
    h      = length rs
    assocs = [((x,y),c) | (y,r) <- zip [0..] rs, (x,c) <- zip [0..] r]
