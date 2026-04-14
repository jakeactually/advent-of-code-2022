module Day25.A (main) where

main :: IO ()
main = do
  content <- readFile "input.txt"
  let theLines = lines content
  -- mapM_ (print . wordToNumber) theLines
  print $ sum $ map wordToNumber theLines

charToNumber :: Char -> Int
charToNumber chr = case chr of
  '2' -> 2
  '1' -> 1
  '0' -> 0
  '-' -> -1
  '=' -> -2
  _ -> error "Invalid character"

powersOf5 :: [Int]
powersOf5 = map (5 ^) [0..]

wordToNumber :: String -> Int
wordToNumber word = sum $ zipWith (*) coeffs powersOf5
  where
    coeffs = map charToNumber $ reverse word
