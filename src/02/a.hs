{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Text (pack, splitOn, unpack)
import Data.Tuple (swap)

data Hand = Rock | Paper | Scissors
    deriving Enum

charToHand :: Char -> Hand
charToHand c = case c of
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors

lineToPlay :: [Char] -> (Hand, Hand)
lineToPlay line = (charToHand a, charToHand b)
    where
        [a, _, b] = line

beat :: Hand -> Hand -> Int
beat Rock Paper = 0
beat Rock Rock = 3
beat Rock Scissors = 6
beat Paper Scissors = 0
beat Paper Paper = 3
beat Paper Rock = 6
beat Scissors Rock = 0
beat Scissors Scissors = 3
beat Scissors Paper = 6

score :: (Hand, Hand) -> Int
score (p1, p2) = fromEnum p1 + 1 + beat p1 p2

main :: IO ()
main = do
    theLines <- lines <$> readFile "input.txt"
    let pairs = map (score . swap . lineToPlay) theLines
    print $ sum pairs
