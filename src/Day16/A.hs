module Day16.A where

import qualified Data.Map.Strict as M
import Day16.Util (precompute, readPuzzle, visit)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let puzzle = readPuzzle input
  let (_, flows, indices, distances) = precompute puzzle
  let part1Map = visit "AA" 30 0 0 flows indices distances
  let part1 = maximum (0 : M.elems part1Map)
  putStrLn "Day 16 Part A"
  print part1
