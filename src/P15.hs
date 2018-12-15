{-# LANGUAGE OverloadedStrings #-}

module P15 where

import Util
import qualified Data.Array.Unboxed as A

type RogueMap = A.Array Point Char

p15 :: IO ()
p15 = do
  input <- lines <$> slurp 1015
  let raw = parseRogue input
  printRogueMap raw
  putStrLn ""
  mapM_ putStrLn input

printRogueMap :: RogueMap -> IO ()
printRogueMap = (mapM_ putStrLn) . showRogueMap

showRogueMap rm =
    let ((xmin,ymin),(xmax,ymax)) = A.bounds rm
        rows = map (\y -> [(x,y) | x <- [xmin..xmax]]) [ymin..ymax]
    in map (map (\k -> rm A.! k)) rows

----------- parsers ---------------

parseRogue :: [String] -> RogueMap
parseRogue rs = A.array ((0,0),(h-1,w-1)) assocs
  where
    w      = length (head rs)
    h      = length rs
    assocs = [((x,y),c) | (y,r) <- zip [0..] rs, (x,c) <- zip [0..] r]
