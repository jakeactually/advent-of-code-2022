{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day17.B where

-- based on this solution https://www.reddit.com/r/adventofcode/comments/znykq2/comment/j3jazwg/

import qualified Data.Map as Map
import Data.Map (Map)

type Coord = (Int, Int)
type Rock = [Coord]

-- state key: (jet index mod len, rock type 0..4, rock x position when landed)
type StateKey = (Int, Int, Int)
type StateValue = (Int, Int)  -- (n_rock, height)

totalRocks :: Int
totalRocks = 1000000000000 + 1

rocks :: [Rock]
rocks =
  [ -- ####
    [(0, 0), (1, 0), (2, 0), (3, 0)]
  , -- .#.
    -- ###
    -- .#.
    [(1, 2), (0, 1), (1, 1), (2, 1), (1, 0)]
  , -- ..#
    -- ..#
    -- ###
    [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)]
  , -- #
    -- #
    -- #
    -- #
    [(0, 0), (0, 1), (0, 2), (0, 3)]
  , -- ##
    -- ##
    [(0, 0), (1, 0), (0, 1), (1, 1)]
  ]

main :: IO ()
main = do
  directions <- readFile "input.txt"
  let indexedJets = cycle $ zip [0..] directions
      mapWithFloor = foldr (\x acc -> Map.insert (x, 0) True acc) Map.empty [0..6]
      initialRock = map (\(x, y) -> (x + 2, y + 4)) (head rocks)
      initialMap = mapWithFloor
      initialRockIndex = 1
      (finalHeight, _) = tick initialMap initialRock initialRockIndex indexedJets Map.empty
  print finalHeight

moveRock :: Map Coord Bool -> Rock -> Char -> Rock
moveRock m rock '<' = if any (\(x, y) -> x == 0 || Map.findWithDefault False (x - 1, y) m) rock
  then rock
  else map (\(x, y) -> (x - 1, y)) rock
moveRock m rock '>' = if any (\(x, y) -> x == 6 || Map.findWithDefault False (x + 1, y) m) rock
  then rock
  else map (\(x, y) -> (x + 1, y)) rock

rockFalls :: Rock -> Rock
rockFalls = map (\(x, y) -> (x, y - 1))

rockCrashes :: Map Coord Bool -> Rock -> Bool
rockCrashes m = any (\coord -> Map.findWithDefault False coord m)

-- One physics step: apply jet, fall, check crash. Returns (crashed, landedRock, newMap, height, nextRock, nextRockIndex).
step
  :: Map Coord Bool
  -> Rock
  -> Int
  -> Char
  -> (Bool, Rock, Map Coord Bool, Int, Rock, Int)
step m rock rockIndex d =
  let movedRock = moveRock m rock d
      fellRock = rockFalls movedRock
      crashed = rockCrashes m fellRock
      newMap = if crashed then foldr (`Map.insert` True) m movedRock else m
      currentHeight = maximum (map snd (Map.keys newMap))
      nextRock = if crashed
        then map (\(x, y) -> (x + 2, y + currentHeight + 4)) (rocks !! mod rockIndex 5)
        else fellRock
      nextRockIndex = if crashed then rockIndex + 1 else rockIndex
  in (crashed, movedRock, newMap, currentHeight, nextRock, nextRockIndex)

handleCycleDetection :: Int -> Int -> Map StateKey StateValue -> (Int,  Int) -> Maybe (Int, Map StateKey StateValue)
handleCycleDetection currentHeight rockIndex st (prevRock, prevHeight) =
  let rcycle = rockIndex - prevRock
      hcycle = currentHeight - prevHeight
      (more, remain) = (totalRocks - rockIndex - 1) `divMod` rcycle
  in if remain == 0
     then Just (hcycle * more + currentHeight, st)
     else Nothing

tick
  :: Map Coord Bool
  -> Rock
  -> Int
  -> [(Int, Char)]
  -> Map StateKey StateValue
  -> (Int, Map StateKey StateValue)
tick m rock rockIndex ((jetI, d) : ds) states
  | rockIndex == totalRocks = (maximum (map snd (Map.keys m)), states)
  | not crashed = tick newMap nextRock nextRockIndex ds states
  | otherwise = onLand states
  where
    (crashed, movedRock, newMap, currentHeight, nextRock, nextRockIndex) = step m rock rockIndex d
    stateKey = (jetI, (rockIndex - 1) `mod` 5, minimum (map fst movedRock))
    stateVal = (rockIndex, currentHeight)
    states' = Map.insert stateKey stateVal states
    onLand st = case Map.lookup stateKey st >>= handleCycleDetection currentHeight rockIndex st of
      Just result -> result
      Nothing -> tick newMap nextRock nextRockIndex ds states'
