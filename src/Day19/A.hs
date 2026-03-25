module Day19.A (main) where

import Text.Regex.PCRE
import qualified Data.Map as M
import Data.List.Split (chunksOf)

data Element = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

type Blueprint = M.Map Element [(Int, Element)]

parseBlueprint :: [Int] -> Blueprint
parseBlueprint [_, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs] =
    M.fromList [
        (Ore, [(ore_ore, Ore)]),
        (Clay, [(clay_ore, Ore)]),
        (Obsidian, [(obs_ore, Ore), (obs_clay, Clay)]),
        (Geode, [(geo_ore, Ore), (geo_obs, Obsidian)])
    ]
parseBlueprint _ = error "Invalid blueprint format"

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ints = getAllTextMatches (content =~ "[-+]?\\d+") :: [String]
      parsedInts = map read ints :: [Int]
      blueprints = map parseBlueprint (chunksOf 7 parsedInts)
  print blueprints
