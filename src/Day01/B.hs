{-# LANGUAGE OverloadedStrings #-}

module Day01.B where

import Data.Text (pack, splitOn, unpack)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Ord

main :: IO ()
main = do
    text <- readFile "input.txt"
    let chunks = splitOn "\n\n" $ pack text
    let parse = map (read . unpack) . filter (/= "") . splitOn "\n"
    let intChunks = map parse chunks :: [[Int]]
    let greatest = sum $ take 3 $ sortBy (comparing Data.Ord.Down) (map sum intChunks)
    print greatest
