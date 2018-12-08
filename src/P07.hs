{-# LANGUAGE OverloadedStrings #-}

module P07 where

import Util
import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

type SleighStep = (Char, Char)
type SleighDict = M.Map Char [Char]
data WorkState = WS {
    time :: Int,
    workers :: Int,
    soFar :: [(Char, Int)],
    remaining :: [Char]
  } deriving (Show)

p07 :: IO ()
p07 = do
  input <- lines <$> slurp 107
  let dependencies = rights $ map (runParser parseSleighStep "") input
      allKeys = map fst dependencies ++ map snd dependencies |> sort |> nub
      backwardsMap = -- given a key, what steps must happen first?
        allKeys
        |> map (\k -> (k, map fst $ filter ((== k) . snd) dependencies))
        |> M.fromList
      start =
        allKeys
        |> map (\k -> (k, M.lookup k backwardsMap))
        |> filter ((== Just "") . snd)
        |> head
        |> fst
      part1 = walk (const 1) backwardsMap 1 allKeys start
      part2 = walk coster2 backwardsMap 2 allKeys start
  mapM_ print part1
  putStrLn ""
  mapM_ print part2

coster2 :: Char -> Int
coster2 =
  let m = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]
          |> M.fromList
  in (\c -> M.lookup c m |> fromJust)

-- walk :: (Char -> Int) -> SleighDict -> Int -> [Char] -> Char -> (String, Int)
walk coster g numWorkers allKeys start =
  iterate (step coster g)
          WS { time = 1, workers = numWorkers, soFar = [(start, coster start)], remaining = (allKeys \\ [start]) }
  |> takeWhile (\ws -> not . null $ remaining ws)

step :: (Char -> Int) -> SleighDict -> WorkState -> WorkState
step coster g workState =
  if null $ remaining workState
  then workState
  else
    let (ready, pending) = soFar workState
                           |> partition (\(_,t) -> t <= (time workState))
                           |> (\(r,p) -> (map fst r, p))
        nexts = remaining workState
                |> filter (\r -> all ((flip elem) ready) (fromJust $ M.lookup r g)) -- only those whose precursers are all in soFar
                |> sort
                |> take ((workers workState) - length pending)
                |> reverse
                |> map (\k -> (k, (time workState) + (coster k)))
        tick = if null nexts
               then pending
                    |> map snd
                    |> minimum
               else nexts
                    |> map snd
                    |> minimum
    in WS {
            time = tick,
            workers = workers workState,
            soFar = (nexts ++ (soFar workState)),
            remaining = (remaining workState) \\ (map fst nexts)
          }

------------ parsers --------------
parseSleighStep :: Parser SleighStep
parseSleighStep = do
  string "Step "
  first <- upperChar
  string " must be finished before step "
  next <- upperChar
  many anyChar
  return (first, next)
