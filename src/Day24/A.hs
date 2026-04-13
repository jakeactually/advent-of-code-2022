module Day24.A (main) where

main :: IO ()
main = do
  content <- readFile "input.txt"
  let _theLines = lines content
  putStrLn "Day 24 Part A scaffold ready."
