module Day16.B where

import qualified Data.Map.Strict as M
import Data.Bits ((.&.))
import Day16.Util (precompute, readPuzzle, visit)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let puzzle = readPuzzle input
  let (_, flows, indices, distances) = precompute puzzle
  let visited2 = visit "AA" 26 0 0 flows indices distances
  let part2Pairs = [v1 + v2 | (bm1, v1) <- M.assocs visited2, (bm2, v2) <- M.assocs visited2, bm1 .&. bm2 == 0]
  let part2 = maximum (0 : part2Pairs)
  putStrLn "Day 16 Part B"
  print part2
