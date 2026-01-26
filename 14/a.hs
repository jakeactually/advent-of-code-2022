import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (foldl')
import Data.Char (toLower)

-- Point type
type Point = (Int, Int)

-- Parse a single point from string like "470,56"
parsePoint :: String -> Point
parsePoint s = let (xStr, ',':yStr) = break (== ',') s
               in (read xStr, read yStr)

-- Parse a path from string like "470,56 -> 470,54 -> 472,56"
parsePath :: String -> [Point]
parsePath line = map parsePoint $ filter (/= "->") $ words line

-- Generate all points between two points (inclusive)
generateLine :: Point -> Point -> [Point]
generateLine (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = error "Diagonal lines not supported"

-- Generate all points for a path
generatePathPoints :: [Point] -> [Point]
generatePathPoints [] = []
generatePathPoints [p] = [p]
generatePathPoints (p1:p2:rest) = generateLine p1 p2 ++ generatePathPoints (p2:rest)

-- Find bounds of the map
findBounds :: [Point] -> (Point, Point)
findBounds points = let xs = map fst points
                        ys = map snd points
                    in ((minimum xs, minimum ys), (maximum xs, maximum ys))

-- Create map representation
createMap :: [Point] -> Map.Map Point Char
createMap points = Map.fromList [(p, '#') | p <- points]

-- Print map to string
printMap :: (Point, Point) -> Map.Map Point Char -> String
printMap ((minX, minY), (maxX, maxY)) grid = unlines [row y | y <- [minY..maxY]]
  where
    row y = [Map.findWithDefault '.' (x, y) grid | x <- [minX..maxX]]

-- Simulate sand falling from a point
simulateSand :: Point -> Int -> Map.Map Point Char -> Map.Map Point Char
simulateSand source targetX grid = dropSandLoop grid 0
  where
    dropSandLoop :: Map.Map Point Char -> Int -> Map.Map Point Char
    dropSandLoop currentGrid count = 
      case dropSingleSand source currentGrid of
        Nothing -> currentGrid  -- Sand fell off the map
        Just newGrid -> if any (\(x, _) -> x == targetX) (Map.keys newGrid)
                        then newGrid  -- Sand reached x=367
                        else dropSandLoop newGrid (count + 1)

-- Drop a single sand grain
dropSingleSand :: Point -> Map.Map Point Char -> Maybe (Map.Map Point Char)
dropSingleSand startPos grid = case findSandRestPosition startPos grid of
  Nothing -> Nothing  -- Sand fell off the map
  Just restPos -> Just $ Map.insert restPos 'o' grid

-- Find where a sand grain comes to rest
findSandRestPosition :: Point -> Map.Map Point Char -> Maybe Point
findSandRestPosition (x, y) grid
  | y > maxY = Nothing  -- Sand fell off the map
  | otherwise = case findNextPosition (x, y) grid of
      Nothing -> Just (x, y)  -- Sand comes to rest here
      Just nextPos -> findSandRestPosition nextPos grid
  where
    (_, (_, maxY)) = findBounds (Map.keys grid)

-- Find next position for sand
findNextPosition :: Point -> Map.Map Point Char -> Maybe Point
findNextPosition (x, y) grid = 
  let down = (x, y + 1)
      downLeft = (x - 1, y + 1)
      downRight = (x + 1, y + 1)
  in if not (Map.member down grid)
     then Just down
     else if not (Map.member downLeft grid)
          then Just downLeft
          else if not (Map.member downRight grid)
               then Just downRight
               else Nothing

main :: IO ()
main = do
  content <- readFile "input.txt"
  let lines' = lines content
      paths = map parsePath lines'
      allPoints = concatMap generatePathPoints paths
      bounds = findBounds allPoints
      initialGrid = createMap allPoints
      -- Add the starting point at y=0, x=500
      gridWithStart = Map.insert (500, 0) 'S' initialGrid
      -- Simulate sand falling from (500, 0) until reaching x=367
      finalGrid = simulateSand (500, 0) 367 gridWithStart
      sandGrains = length $ filter (== 'o') $ Map.elems finalGrid
      output = printMap bounds finalGrid
  
  print bounds
  putStr output
  putStrLn $ "Total sand grains that came to rest: " ++ show sandGrains