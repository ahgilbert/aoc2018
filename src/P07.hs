{-# LANGUAGE OverloadedStrings #-}

module P07 where

import Util
import Data.Array
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Graph as G
import Text.Megaparsec
import Text.Megaparsec.Char

type SleighStep = (Char, Char)

p07 :: IO ()
p07 = do
  input <- lines <$> slurp 107
  let dependencies = rights $ map (runParser parseSleighStep "") input
      (graph, allKeys, fromVertex, getSuccessors) = mkGraph dependencies
      indegrees = G.indegree graph
      start = allKeys \\ (map snd dependencies) |> head
      ordering = nextStep getSuccessors ([],"C")
  print ordering

mkGraph :: [SleighStep] -> (G.Graph, [Char], (G.Vertex -> (Char, Char, [Char])), (Char -> [Char]))
mkGraph steps =
  let allKeys = map fst steps ++ map snd steps |> nub |> sort
      graphSeed = allKeys
                  |> map (\k -> (k,k,(filter (\(f,_) -> f == k) steps |> map snd)))
      (graph, fromVertex, fromKey) =
        allKeys
        |> map (\k -> (k, k, (filter (\(f,_) -> f == k) steps |> map snd)))
        |> G.graphFromEdges
      getSuccessors k =
        fromKey k
        |> fmap fromVertex
        |> fmap (\(_,_,v) -> v)
        |> fromJust
  in (graph, allKeys, fromVertex, getSuccessors)

walk :: (Char -> [Char]) -> Char -> [Char]
walk stepper start =
  nextStep stepper ([],[start])
  |> fst
  |> reverse

nextStep :: (Char -> [Char]) -> ([Char],[Char]) -> ([Char],[Char])
nextStep _ (soFar, []) = (soFar, [])
nextStep stepper (soFar, candidates) =
  let here = head candidates
      cand' = nub $ sort ((stepper here) ++ (tail candidates))
  in nextStep stepper ((here:soFar), cand')

------------ parsers --------------
parseSleighStep :: Parser SleighStep
parseSleighStep = do
  string "Step "
  first <- upperChar
  string " must be finished before step "
  next <- upperChar
  many anyChar
  return (first, next)
