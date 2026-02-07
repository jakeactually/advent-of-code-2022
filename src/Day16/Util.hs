{-# LANGUAGE FlexibleContexts #-}

module Day16.Util
  ( Valve,
    readPuzzle,
    precompute,
    visit,
  )
where

-- just copied this www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j2xhog7/
-- using cursor to translate to haskell

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Map.Strict (Map, assocs, fromList, insertWith, keys, singleton, unionWith, (!))
import Text.Regex.PCRE

type Valve = String

-- Parse: each line gives [valve, flow, neighbor1, neighbor2, ...]
readPuzzle :: String -> [[String]]
readPuzzle = map parseLine . lines
  where
    parseLine line =
      let twoLetters = getAllTextMatches (line =~ "[A-Z][A-Z]") :: [String]
          numbers = getAllTextMatches (line =~ "\\d+") :: [String]
      in case (twoLetters, numbers) of
           (valve : leads, flow : _) -> valve : flow : leads
           _ -> error $ "parse error: " ++ line

-- (graph, flows, indices, distances)
precompute :: [[String]] -> (Map Valve [String], Map Valve Int, Map Valve Int, Map (Valve, Valve) Int)
precompute puzzle =
  let graph = fromList [(valve, leads) | valve : _ : leads <- puzzle]
      flows = fromList [(valve, read flow) | valve : flow : _ <- puzzle, flow /= "0"]
      flowValves = keys flows
      indices = fromList [(v, (1 :: Int) `shiftL` i) | (i, v) <- zip [0 ..] flowValves]
      nodes = keys graph
      dist0 =
        fromList
          [ ((v, l), if v == l then 0 else if l `elem` (graph ! v) then 1 else 1000)
          | v <- nodes
          , l <- nodes
          ]
      distances = floydWarshall nodes graph dist0
  in (graph, flows, indices, distances)

floydWarshall :: [Valve] -> Map Valve [String] -> Map (Valve, Valve) Int -> Map (Valve, Valve) Int
floydWarshall nodes _ dist = foldl' step dist nodes
  where
    step d k = foldl' (\d' i -> foldl' (\d'' j -> insertWith min (i, j) (d'' ! (i, k) + d'' ! (k, j)) d'') d' nodes) d nodes

-- DFS: from (valve, minutes, bitmask, pressure) returns Map bitmask -> max pressure
visit ::
  Valve ->
  Int ->
  Int ->
  Int ->
  Map Valve Int ->
  Map Valve Int ->
  Map (Valve, Valve) Int ->
  Map Int Int
visit valve minutes bitmask pressure flows indices distances =
  let thisVal = singleton bitmask pressure
      nexts =
        [ (valve2, remainingMins, bitmask', pressure')
        | (valve2, flow) <- assocs flows
        , let remainingMins = minutes - (distances ! (valve, valve2)) - 1
        , remainingMins > 0
        , (indices ! valve2) .&. bitmask == 0
        , let bitmask' = bitmask .|. (indices ! valve2)
        , let pressure' = pressure + flow * remainingMins
        ]
      childMaps = map (\(v2, mins, bm', pr') -> visit v2 mins bm' pr' flows indices distances) nexts
  in foldl' (unionWith max) thisVal childMaps
