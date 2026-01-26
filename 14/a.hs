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

main :: IO ()
main = do
  content <- readFile "input.txt"
  let lines' = lines content
      paths = map parsePath lines'
      allPoints = concatMap generatePathPoints paths
      bounds = findBounds allPoints
      grid = createMap allPoints
      output = printMap bounds grid
  
  print bounds
  putStrLn "Map with path points filled with '#':"
  putStr output
  putStrLn $ "Total points filled: " ++ show (length allPoints)