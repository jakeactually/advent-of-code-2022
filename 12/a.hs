import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char (ord)
import Data.List (find)
import Data.Maybe (isJust)
import Control.Monad (guard)

type Walker = ((Int, Int), Set (Int, Int), [(Int, Int)])

parseInput :: String -> Map (Int, Int) Char
parseInput input = Map.fromList $ do
    (row, line) <- zip [0..] (lines input)
    (col, char) <- zip [0..] line
    return ((row, col), char)

getHeight :: Char -> Int
getHeight 'S' = ord 'a'
getHeight 'E' = ord 'z'
getHeight c = ord c

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

generateNewWalkers :: Map (Int, Int) Char -> (Int, Int) -> Set (Int, Int) -> [(Int, Int)] -> [Walker]
generateNewWalkers grid pos visited path = do
    npos <- neighbors pos
    guard (isJust (Map.lookup npos grid) &&
           getHeight (grid Map.! npos) <= getHeight (grid Map.! pos) + 1 &&
           not (Set.member npos visited))
    return (npos, Set.insert npos visited, npos : path)

bfs :: Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
bfs grid start endPos = go [(start, Set.singleton start, [start])]
  where
    go :: [Walker] -> Maybe [(Int, Int)]
    go [] = Nothing
    go ((pos, visited, path):queue)
        | pos == endPos = Just (reverse path)
        | otherwise = go (queue ++ generateNewWalkers grid pos visited path)

findPath :: Map (Int, Int) Char -> Maybe [(Int, Int)]
findPath grid = bfs grid start endPos
  where
    start = head [pos | (pos, 'S') <- Map.toList grid]
    endPos = head [pos | (pos, 'E') <- Map.toList grid]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = parseInput input
    case findPath grid of
        Just path -> do
            putStrLn "Path found:"
            print path
            putStrLn ("Length: " ++ show (length path - 1))
        Nothing -> putStrLn "No path found"
