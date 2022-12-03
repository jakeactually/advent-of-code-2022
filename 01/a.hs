{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, splitOn, unpack)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let chunks = splitOn "\n\n" $ pack text
    let parse = map (read . unpack) . filter (/= "") . splitOn "\n"
    let intChunks = map parse chunks :: [[Int]]
    let greatest = maximum $ map sum intChunks
    print greatest
