module Day19.A (main) where

main :: IO ()
main = do
  content <- readFile "input.txt"
  let _theLines = lines content
  putStrLn "Day 19 Part A scaffold ready."
