module Day25.B (main) where

main :: IO ()
main = do
  content <- readFile "input.txt"
  let _theLines = lines content
  putStrLn "Day 25 Part B scaffold ready."
