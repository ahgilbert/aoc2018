{-# LANGUAGE OverloadedStrings #-}

module P18 where

import Util
import qualified Data.Array.Unboxed as A

type Settlers = A.Array Point Industry
data Industry = Open | Wooded | Lumberyard
  deriving (Eq)
instance Show Industry where
  show Open = "."
  show Wooded = "|"
  show Lumberyard = "#"

p18 :: IO ()
p18 = do
  settlers <- parseSettlers <$> lines <$> slurp 18
  let gens = iterate evolve settlers
      spaced = map (\i -> (i, gens !! i)) [1000,2000..]
  mapM_ print $ (map . fmap) scoreSettlers (take 50 spaced)

scoreSettlers :: Settlers -> Int
scoreSettlers s =
  let land = A.elems s
      numWooded = land
                  |> filter (== Wooded)
                  |> length
      numLumber = land
                  |> filter (== Lumberyard)
                  |> length
  in numWooded * numLumber

evolve :: Settlers -> Settlers
evolve s = map (step s) (A.indices s)
           |> A.listArray (A.bounds s)

step :: Settlers -> Point -> Industry
step sett p =
  let here = sett A.! p
      neighbors = neighbors8 p
      neighborhood = map (getSett sett) neighbors
  in grow here neighborhood

grow :: Industry -> [Industry] -> Industry
grow Open ns =
  let nTrees = ns |> filter (== Wooded) |> length
  in if nTrees >= 3 then Wooded else Open
grow Wooded ns =
  let nYards = ns |> filter (== Lumberyard) |> length
  in if nYards >= 3 then Lumberyard else Wooded
grow Lumberyard ns =
  let hasTrees = ns |> filter (== Wooded) |> length |> (>= 1)
      hasLumber = ns |> filter (== Lumberyard) |> length |> (>= 1)
  in if hasTrees && hasLumber then Lumberyard else Open

------------ utils -----------------

getSett :: Settlers -> Point -> Industry
getSett sett p =
  let ((xmin,ymin),(xmax,ymax)) = A.bounds sett
      inbounds = xmin <= fst p && xmax >= fst p && ymin <= snd p && ymax >= snd p
  in if inbounds
     then sett A.! p
     else Open

printSettlers = (mapM_ putStrLn) . showSettlers

showSettlers :: Settlers -> [String]
showSettlers s =
  let ((xmin,ymin),(xmax,ymax)) = A.bounds s
      rows = map (\y -> [(y,x) | x <- [xmin..xmax]]) [ymin..ymax]
  in map (\r -> concat $ map (\p -> show $ s A.! p) r) rows

------------ parsers ---------------

parseIndustry '.' = Open
parseIndustry '|' = Wooded
parseIndustry '#' = Lumberyard
parseIndustry  _  = undefined

parseSettlers :: [String] -> Settlers
parseSettlers rs = A.array ((0,0),(h-1,w-1)) assocs
  where
    w      = length (head rs)
    h      = length rs
    assocs = [((y,x), parseIndustry c) | (y,r) <- zip [0..] rs, (x,c) <- zip [0..] r]
