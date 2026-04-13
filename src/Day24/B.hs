module Day24.B (main) where

main :: IO ()
main = do
  content <- readFile "input.txt"
  let _theLines = lines content
  putStrLn "Day 24 Part B scaffold ready."
