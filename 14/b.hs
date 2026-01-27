import Control.Monad (forM_, when, unless)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.MArray
import Data.List (foldl')
import Data.STRef

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

solve :: [Point] -> (Int, UArray Point Char)
solve points = runST $ do
    let ((minX_rocks, minY_rocks), (maxX_rocks, maxY_rocks)) = findBounds points
    let floorY = maxY_rocks + 2
    -- Sand starts at 500,0. Max spread is floorY.
    -- So range [500 - floorY - 2, 500 + floorY + 2] covers all reachable points.
    let minX = minimum [minX_rocks, 500 - floorY - 2]
    let maxX = maximum [maxX_rocks, 500 + floorY + 2]
    let minY = 0
    let maxY = floorY
    
    -- Create mutable array
    grid <- newArray ((minX, minY), (maxX, maxY)) '.' :: ST s (STUArray s Point Char)
    
    -- Draw rocks
    forM_ points $ \p -> writeArray grid p '#'
    
    -- Draw floor
    forM_ [minX..maxX] $ \x -> writeArray grid (x, floorY) '#'
    
    countRef <- newSTRef 0
    
    let loop = do
            -- Drop sand from source
            settled <- dropSand grid (500, 0) floorY
            case settled of
                Nothing -> return () -- Should not happen with infinite floor
                Just p -> do
                    writeArray grid p 'o'
                    modifySTRef' countRef (+1)
                    if p == (500, 0)
                        then return ()
                        else loop
    
    loop
    
    finalCount <- readSTRef countRef
    finalGrid <- freeze grid
    return (finalCount, finalGrid)

dropSand :: STUArray s Point Char -> Point -> Int -> ST s (Maybe Point)
dropSand grid (x, y) floorY
    | y + 1 > floorY = return Nothing -- Hit floor (should be caught by floor check)
    | otherwise = do
        -- Check down
        cDown <- readArray grid (x, y+1)
        if cDown == '.' then dropSand grid (x, y+1) floorY
        else do
            -- Check down-left
            cLeft <- readArray grid (x-1, y+1)
            if cLeft == '.' then dropSand grid (x-1, y+1) floorY
            else do
                -- Check down-right
                cRight <- readArray grid (x+1, y+1)
                if cRight == '.' then dropSand grid (x+1, y+1) floorY
                else return (Just (x, y))

printMap :: (Point, Point) -> UArray Point Char -> String
printMap ((minX, minY), (maxX, maxY)) grid = unlines [row y | y <- [minY..maxY]]
  where
    row y = [grid ! (x, y) | x <- [minX..maxX]]

main :: IO ()
main = do
  content <- readFile "input.txt"
  let lines' = lines content
      paths = map parsePath lines'
      allPoints = concatMap generatePathPoints paths
      ((sx, sy), (ex, ey)) = findBounds allPoints
      
      (sandGrains, finalGrid) = solve allPoints
      
      -- Print map around rocks like original
      output = printMap ((sx, sy), (ex, ey)) finalGrid
  
  print ((sx, sy), (ex, ey))
  putStr output
  putStrLn $ "Total sand grains that came to rest: " ++ show sandGrains
