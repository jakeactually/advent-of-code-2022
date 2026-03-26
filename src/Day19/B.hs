module Day19.B (main) where

import Data.List (sortBy)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.Regex.PCRE

data Element = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

type Blueprint = M.Map Element [(Int, Element)]

parseBlueprint :: [Int] -> Blueprint
parseBlueprint [_, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs] =
  M.fromList
    [ (Ore, [(ore_ore, Ore)]),
      (Clay, [(clay_ore, Ore)]),
      (Obsidian, [(obs_ore, Ore), (obs_clay, Clay)]),
      (Geode, [(geo_ore, Ore), (geo_obs, Obsidian)])
    ]
parseBlueprint _ = error "Invalid blueprint format"

data Walker = Walker
  { inventory :: M.Map Element Int,
    robots :: M.Map Element Int,
    target :: Element
  }
  deriving (Show, Eq, Ord)

stepWalker :: M.Map Element Int -> Blueprint -> Walker -> [Walker]
stepWalker maxC bp (Walker inv robs tgt) =
  let cost = M.findWithDefault [] tgt bp
      canAfford = all (\(amt, e) -> M.findWithDefault 0 e inv >= amt) cost
   in if canAfford
        then
          let invAfterPay = foldl (\acc (amt, e) -> M.insertWith (+) e (-amt) acc) inv cost
              invNext = foldl (\acc (e, amt) -> M.insertWith (+) e amt acc) invAfterPay (M.toList robs)
              robsNext = M.insertWith (+) tgt 1 robs
              possibleTargets =
                [ t
                | t <- [Ore, Clay, Obsidian, Geode],
                  let tcost = M.findWithDefault [] t bp,
                  all (\(_, ce) -> M.findWithDefault 0 ce robsNext > 0) tcost,
                  t == Geode || M.findWithDefault 0 t robsNext < M.findWithDefault (maxBound :: Int) t maxC
                ]
           in [Walker invNext robsNext newTgt | newTgt <- possibleTargets]
        else
          let invNext = foldl (\acc (e, amt) -> M.insertWith (+) e amt acc) inv (M.toList robs)
           in [Walker invNext robs tgt]

runBlueprint :: Int -> Blueprint -> Int
runBlueprint minutes bp =
  let allCosts = concat (M.elems bp)
      maxC = M.fromListWith max [(e, amt) | (amt, e) <- allCosts]
      robsStart = M.singleton Ore 1
      invStart = M.empty
      possibleTargets =
        [ t
        | t <- [Ore, Clay, Obsidian, Geode],
          let tcost = M.findWithDefault [] t bp,
          all (\(_, ce) -> M.findWithDefault 0 ce robsStart > 0) tcost,
          t == Geode || M.findWithDefault 0 t robsStart < M.findWithDefault (maxBound :: Int) t maxC
        ]
      initialWalkers = [Walker invStart robsStart newTgt | newTgt <- possibleTargets]

      loop 0 ws = ws
      loop m ws =
        let currentMaxGeodes = maximum (0 : map (\w -> M.findWithDefault 0 Geode (inventory w)) ws)
            filteredWs = filter (\w -> M.findWithDefault 0 Geode (inventory w) >= currentMaxGeodes) ws
         in loop (m - 1) (S.toList . S.fromList $ concatMap (stepWalker maxC bp) filteredWs)

      finalWalkers = loop minutes initialWalkers
      maxGeodes = maximum (0 : map (\w -> M.findWithDefault 0 Geode (inventory w)) finalWalkers)
      bestWalkers = take 1 $ sortBy (\w1 w2 -> compare (M.findWithDefault 0 Geode $ inventory w2) (M.findWithDefault 0 Geode $ inventory w1)) finalWalkers
   in trace ("Blueprint resulted in " ++ show (length finalWalkers) ++ " final walkers.\nTop Walker: " ++ show bestWalkers) maxGeodes

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ints = getAllTextMatches (content =~ "[-+]?\\d+") :: [String]
      parsedInts = map read ints :: [Int]
      blueprints = map parseBlueprint $ take 3 (chunksOf 7 parsedInts)

  let results = map (runBlueprint 32) blueprints
  print results
