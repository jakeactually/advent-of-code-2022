{-# LANGUAGE FlexibleContexts #-}

-- just copied this www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j2xhog7/
-- using cursor to translate to haskell

module Day16.A where

import Data.Map.Strict (Map, assocs, fromList, insertWith, keys, singleton, unionWith, (!))
import qualified Data.Map.Strict as M
import Data.Bits ((.&.), (.|.), shiftL)
import Text.Regex.PCRE

type Valve = String

-- Parse: each line gives [valve, flow, neighbor1, neighbor2, ...]
-- Regex [A-Z][A-Z] gives valve names in order; \d+ gives flow.
readPuzzle :: String -> [[String]]
readPuzzle = map parseLine . lines
  where
    parseLine line =
      let twoLetters = getAllTextMatches (line =~ "[A-Z][A-Z]") :: [String]
          numbers = getAllTextMatches (line =~ "\\d+") :: [String]
      in case (twoLetters, numbers) of
           (valve : leads, flow : _) -> valve : flow : leads
           _ -> error $ "parse error: " ++ line

solvePuzzle :: [[String]] -> (Int, Int)
solvePuzzle puzzle =
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
      visit :: Valve -> Int -> Int -> Int -> Map Int Int
      visit valve minutes bitmask pressure =
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
            childMaps = map (\(v2, mins, bm', pr') -> visit v2 mins bm' pr') nexts
        in foldl' (unionWith max) thisVal childMaps
      part1Map = visit "AA" 30 0 0
      part1 = maximum (0 : M.elems part1Map)
      visited2 = visit "AA" 26 0 0
      part2Pairs = [v1 + v2 | (bm1, v1) <- M.assocs visited2, (bm2, v2) <- M.assocs visited2, bm1 .&. bm2 == 0]
      part2 = maximum (0 : part2Pairs)
  in (part1, part2)

floydWarshall :: [Valve] -> Map Valve [String] -> Map (Valve, Valve) Int -> Map (Valve, Valve) Int
floydWarshall nodes _ dist = foldl' step dist nodes
  where
    step d k = foldl' (\d' i -> foldl' (\d'' j -> insertWith min (i, j) (d'' ! (i, k) + d'' ! (k, j)) d'') d' nodes) d nodes

main :: IO ()
main = do
  input <- readFile "input.txt"
  let puzzle = readPuzzle input
  let (part1, part2) = solvePuzzle puzzle
  putStrLn "Day 16"
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
