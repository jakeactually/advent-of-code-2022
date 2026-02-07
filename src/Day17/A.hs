module Day17.A where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Debug.Trace as Debug

type Coord = (Int, Int)
type Rock = [Coord]

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
  let mapWithFloor = foldr (\x acc -> Map.insert (x, 0) True acc) Map.empty [0..6]
      initialRock = map (\(x, y) -> (x + 2, y + 4)) (rocks !! 0)
      initialMap = mapWithFloor
      initialRockIndex = 1
      finalHeight = tick initialMap initialRock initialRockIndex (cycle directions)
  print finalHeight

moveRock :: Map Coord Bool -> Rock -> Char -> Rock
moveRock m rock '<' = if any (\(x, y) -> x == 0 || Map.findWithDefault False (x - 1, y) m) rock
  then rock
  else map (\(x, y) -> (x - 1, y)) rock
moveRock m rock '>' = if any (\(x, y) -> x == 6 || Map.findWithDefault False (x + 1, y) m) rock
  then rock
  else map (\(x, y) -> (x + 1, y)) rock

rockFalls :: Rock -> Rock
rockFalls rock = map (\(x, y) -> (x, y - 1)) rock

rockCrashes :: Map Coord Bool -> Rock -> Bool
rockCrashes m rock = any (\coord -> Map.findWithDefault False coord m) rock

tick :: Map Coord Bool -> Rock -> Int -> [Char] -> Int
tick m rock rockIndex (d:ds) =
  if rockIndex == 2023
  then topHeight
  else tick newMap newRock newRockIndex ds
  where
    movedRock = moveRock m rock d
    fellRock = rockFalls movedRock
    crashes = rockCrashes m fellRock
    newRock = if crashes
      then map (\(x, y) -> (x + 2, y + topHeight + 4)) (rocks !! (mod rockIndex 5))
      else fellRock
    newMap = if crashes
      then foldr (\coord acc -> Map.insert coord True acc) m movedRock
      else m
    newRockIndex = if crashes
      then rockIndex + 1
      else rockIndex
    topHeight = maximum (map snd (Map.keys newMap))
