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
  let jetLen = length directions
      indexedJets = zip (cycle [0 .. jetLen - 1]) (cycle directions)
      mapWithFloor = foldr (\x acc -> Map.insert (x, 0) True acc) Map.empty [0..6]
      initialRock = map (\(x, y) -> (x + 2, y + 4)) (head rocks)
      initialMap = mapWithFloor
      initialRockIndex = 1
      (mbHeight, _) = tick jetLen totalRocks initialMap initialRock initialRockIndex indexedJets Map.empty
      finalHeight = case mbHeight of
        Just h -> h
        Nothing -> error "expected cycle or completion"
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

tick
  :: Int
  -> Int
  -> Map Coord Bool
  -> Rock
  -> Int
  -> [(Int, Char)]
  -> Map StateKey StateValue
  -> (Maybe Int, Map StateKey StateValue)
tick jetLen total m rock rockIndex ((jetI, d) : ds) states =
  if rockIndex == total + 1
  then (Just topHeight, states)
  else
    let movedRock = moveRock m rock d
        fellRock = rockFalls movedRock
        crashes = rockCrashes m fellRock
        currentHeight = maximum (map snd (Map.keys (if crashes then foldr (`Map.insert` True) m movedRock else m)))
        newRock = if crashes
          then map (\(x, y) -> (x + 2, y + currentHeight + 4)) (rocks !! mod rockIndex 5)
          else fellRock
        newMap = if crashes
          then foldr (`Map.insert` True) m movedRock
          else m
        newRockIndex = if crashes
          then rockIndex + 1
          else rockIndex
    in if crashes
       then
         let stateKey = (jetI, (rockIndex - 1) `mod` 5, minimum (map fst movedRock))
             stateVal = (rockIndex, currentHeight)
             states' = Map.insert stateKey stateVal states
             go = tick jetLen total newMap newRock newRockIndex ds
         in case Map.lookup stateKey states of
              Just (prevRock, prevHeight) ->
                let rcycle = rockIndex - prevRock
                    hcycle = currentHeight - prevHeight
                    diff = total - rockIndex - 1
                    (more, remain) = diff `divMod` rcycle
                in if remain == 0
                   then (Just (hcycle * more + currentHeight), states)
                   else go states'
              Nothing -> go (Map.insert stateKey stateVal states)
       else tick jetLen total newMap newRock newRockIndex ds states
  where
    topHeight = maximum (map snd (Map.keys m))
