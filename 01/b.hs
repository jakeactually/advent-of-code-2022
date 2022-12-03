{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack)
import Data.List (sort)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let chunks = splitOn "\n\n" $ pack text
    let parse = map (read . unpack) . filter (/= "") . splitOn "\n"
    let intChunks = map parse chunks :: [[Int]]
    let greatest = sum $ take 3 $ reverse $ sort $ map sum intChunks
    print greatest
