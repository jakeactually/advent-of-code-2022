module Day18.A where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let theLines = lines content
  let cubes = map (wordsToCoord . splitOn ",") theLines
  let adjacents = filter ((== 1) . uncurry manhattan) (combinations cubes)
  print $ (length cubes) * 6 - (length adjacents) * 2

wordsToCoord :: [String] -> (Int, Int, Int)
wordsToCoord [x, y, z] = (read x, read y, read z)

manhattan :: (Int, Int, Int) -> (Int, Int, Int) -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

combinations :: [a] -> [(a, a)]
combinations (x:xs) = map (\y -> (x, y)) xs ++ combinations xs
combinations [] = []
