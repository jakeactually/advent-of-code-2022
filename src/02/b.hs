{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Text (pack, splitOn, unpack)
import Data.Tuple (swap)

data Hand = Rock | Paper | Scissors
    deriving Enum

data Response = Lose | Draw | Win
    deriving Enum

charToHand :: Char -> Hand
charToHand c = case c of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors

charToResponse :: Char -> Response
charToResponse c = case c of
    'X' -> Lose
    'Y' -> Draw
    'Z' -> Win

lineToPlay :: [Char] -> (Hand, Response)
lineToPlay line = (charToHand a, charToResponse b)
    where
        [a, _, b] = line

play :: Hand -> Response -> Hand
play Rock Lose = Scissors
play Rock Win = Paper
play Paper Lose = Rock
play Paper Win = Scissors
play Scissors Lose = Paper
play Scissors Win = Rock
play x Draw = x

score :: (Hand, Response) -> Int
score (p2, response) = fromEnum p1 + 1 + fromEnum response * 3
    where
        p1 = play p2 response

main :: IO ()
main = do
    theLines <- lines <$> readFile "input.txt"
    let pairs = map (score . lineToPlay) theLines
    print $ sum pairs
