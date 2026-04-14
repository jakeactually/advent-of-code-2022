module Day25.A (main) where

main :: IO ()
main = do
  content <- readFile "input.txt"
  let theLines = lines content
  -- mapM_ (print . wordToNumber) theLines
  print $ encode $ sum $ map wordToNumber theLines

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

-- https://www.reddit.com/r/adventofcode/comments/zur1an/comment/jmfi54t/

encode :: Int -> String
encode 0 = ""
encode number = encode nextNumber ++ [chr]
  where
    chrIdx = (number + 2) `mod` 5
    chr = "=-012" !! chrIdx
    carry = if chrIdx < 2 then 1 else 0
    nextNumber = number `div` 5 + carry