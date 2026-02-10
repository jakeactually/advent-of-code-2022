module Day18.B where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (catMaybes)

type Coord = (Int, Int, Int)
data Material = Air | Water | Solid deriving Show

main :: IO ()
main = do
  content <- readFile "input.txt"
  let theLines = lines content
  let cubes = map (wordsToCoord . splitOn ",") theLines
  let bounds = getMapBoundsWithPadding cubes
  let emptyMap = emptyMapWithAir bounds
  let initialMap = insertCubes emptyMap cubes
  let surface = floodMap initialMap [fst bounds]
  print surface

wordsToCoord :: [String] -> Coord
wordsToCoord [x, y, z] = (read x, read y, read z)

getMapBoundsWithPadding :: [Coord] -> (Coord, Coord)
getMapBoundsWithPadding cs = (start, end)
  where
    start = (minimum xCoords - 1, minimum yCoords - 1, minimum zCoords - 1)
    end = (maximum xCoords + 1, maximum yCoords + 1, maximum zCoords + 1)
    xCoords = map (\(x, _, _) -> x) cs
    yCoords = map (\(_, y, _) -> y) cs
    zCoords = map (\(_, _, z) -> z) cs

checkCoord :: Map Coord Material -> Coord -> (Maybe Coord, Int)
checkCoord m coord = case Map.lookup coord m of
  Just Solid -> (Nothing, 1)
  Just Water -> (Nothing, 0)
  Just Air -> (Just coord, 0)
  _ -> (Nothing, 0)

expandWater :: Map Coord Material -> Coord -> ([Coord], Int)
expandWater m coord = (catMaybes newCoords, sum faces)
  where
    (newCoords, faces) = unzip $ map (checkCoord m) (cubeFaces coord)

floodMap :: Map Coord Material -> [Coord] -> Int
floodMap m coords = if null coords
  then 0
  else newFaces + floodMap newMap newCoords
    where
      (tempCoords, tempFaces) = unzip $ map (expandWater m) coords
      (newCoords, newFaces) = (nub $ concat tempCoords, sum tempFaces)
      newMap = foldl (\m coord -> Map.insert coord Water m) m newCoords

cubeFaces :: Coord -> [Coord]
cubeFaces (x, y, z) =
  [ (x + 1, y, z),
    (x - 1, y, z),
    (x, y + 1, z),
    (x, y - 1, z),
    (x, y, z + 1),
    (x, y, z - 1)
  ]

emptyMapWithAir :: (Coord, Coord) -> Map Coord Material
emptyMapWithAir (start, end) = Map.fromList $ do
  x <- [startX..endX]
  y <- [startY..endY]
  z <- [startZ..endZ]
  return ((x, y, z), Air)
    where
      (startX, startY, startZ) = start
      (endX, endY, endZ) = end

insertCubes :: Map Coord Material -> [Coord] -> Map Coord Material
insertCubes = foldl (\m coord -> Map.insert coord Solid m)
