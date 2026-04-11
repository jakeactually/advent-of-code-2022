module Day22.A (main) where

import Data.List (unsnoc)
import Data.Map (Map, fromList)

main :: IO ()
main = do
  file <- readFile "input.txt"
  let Just (chartLines, path) = unsnoc $ lines file
  let chart = chartLinesToMap $ init chartLines
  print chart

chartLinesToMap :: [String] -> Map (Int, Int) Char
chartLinesToMap chartLines = fromList $ do
  (y, chartLine) <- zip [0 ..] chartLines
  (x, char) <- zip [0 ..] chartLine
  return ((x, y), char)
