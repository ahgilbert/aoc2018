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
  input <- lines <$> slurp 7
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
      part1 = walk (const 1) backwardsMap allKeys start
  print $ fst part1

walk :: (Char -> Int) -> SleighDict -> [Char] -> Char -> (String, Int)
walk coster g allKeys start =
  step
    coster
    g
    WS { time = 1, workers = 1, soFar = [(start, coster start)], remaining = (allKeys \\ [start]) }
  |> (\ws -> (reverse $ map fst (soFar ws), time ws))

step :: (Char -> Int) -> SleighDict -> WorkState -> WorkState
step coster g workState =
  if null $ remaining workState
  then workState
  else
    let ready = soFar workState
                |> filter (\(c,t) -> t <= (time workState))
                |> map fst
        nexts = remaining workState
                |> filter (\r -> all ((flip elem) ready) (fromJust $ M.lookup r g)) -- only those whose precursers are all in soFar
                |> sort
                |> take (workers workState)
                |> reverse
                |> map (\k -> (k, (time workState) + (coster k)))
        tick = nexts
               |> map snd
               |> minimum
    in step coster g (WS {
                             time = tick,
                             workers = ((workers workState) - length nexts + 1),
                             soFar = (nexts ++ (soFar workState)),
                             remaining = (remaining workState) \\ (map fst nexts)
                           })

------------ parsers --------------
parseSleighStep :: Parser SleighStep
parseSleighStep = do
  string "Step "
  first <- upperChar
  string " must be finished before step "
  next <- upperChar
  many anyChar
  return (first, next)
