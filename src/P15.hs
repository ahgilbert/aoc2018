{-# LANGUAGE OverloadedStrings #-}

module P15 where

import Util
import Data.List
import qualified Data.Array.Unboxed as A

type RogueMap = A.Array Point Char

p15 :: IO ()
p15 = do
  input <- lines <$> slurp 1015
  let raw = parseRogue input
      cave = fmap (\c -> if c == 'G' || c == 'E' then '.' else c) raw
  printRogueMap raw
  putStrLn ""
  mapM_ putStrLn input

------- problem logic ----------



----------- utils --------------

getPath :: RogueMap -> Point -> Point -> [Point]
getPath rm start end = undefined

printRogueMap :: RogueMap -> IO ()
printRogueMap = (mapM_ putStrLn) . showRogueMap

showRogueMap rm =
    let ((xmin,ymin),(xmax,ymax)) = A.bounds rm
        rows = map (\y -> [(x,y) | x <- [xmin..xmax]]) [ymin..ymax]
    in map (map (\k -> rm A.! k)) rows

readingOrder :: [Point] -> [Point]
readingOrder ps =
  let comp (xa,ya) (xb,yb) =
        if (compare ya yb) == EQ
        then (compare xa xb)
        else (compare ya yb)
  in sortBy comp ps

----------- parsers ---------------

parseRogue :: [String] -> RogueMap
parseRogue rs = A.array ((0,0),(h-1,w-1)) assocs
  where
    w      = length (head rs)
    h      = length rs
    assocs = [((x,y),c) | (y,r) <- zip [0..] rs, (x,c) <- zip [0..] r]
