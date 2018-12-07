{-# LANGUAGE OverloadedStrings #-}

module P07 where

import Util
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Graph as G
import Text.Megaparsec
import Text.Megaparsec.Char

type SleighStep = (Char, Char)

p07 :: IO ()
p07 = do
  input <- lines <$> slurp 7
  let dependencies = rights $ map (runParser parseSleighStep "") input
      allKeys = map fst dependencies ++ map snd dependencies |> nub
      (graph, fromVertex, fromKey) =
        allKeys
        |> map (\k -> (k, k, (filter (\(f,_) -> f == k) dependencies |> map snd)))
        |> G.graphFromEdges
      getSuccessors = (\(_,_,s) -> s) . fromVertex . fromJust . fromKey
      inDegrees = map (length . getSuccessors) allKeys
                  |> zip allKeys
      start = inDegrees
              |> minimumBy (\(_,a) (_,b) -> compare a b)
              |> fst
  print start
  print $ getSuccessors 'X'

------------ parsers --------------
parseSleighStep :: Parser SleighStep
parseSleighStep = do
  string "Step "
  first <- upperChar
  string " must be finished before step "
  next <- upperChar
  many anyChar
  return (first, next)
