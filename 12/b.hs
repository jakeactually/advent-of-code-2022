import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set, minView)
import Data.Char (ord)
import Data.List (find)
import Data.Maybe (isJust)
import Control.Monad (guard)
import Data.Foldable (foldl')
import Debug.Trace (trace)

type Pos = (Int, Int)

newtype PrioPos = PrioPos (Int, Pos) -- steps, pos

instance Ord PrioPos where
  compare (PrioPos (s1, p1)) (PrioPos (s2, p2)) = compare s1 s2 `mappend` compare p1 p2

instance Eq PrioPos where
  PrioPos (s1, p1) == PrioPos (s2, p2) = s1 == s2 && p1 == p2

parseInput :: String -> Map Pos Char
parseInput input = Map.fromList $ do
  (row, line) <- zip [0..] (lines input)
  (col, char) <- zip [0..] line
  return ((row, col), char)

getHeight :: Char -> Int
getHeight 'S' = ord 'a'
getHeight 'E' = ord 'z'
getHeight c = ord c

neighbors :: Pos -> [Pos]
neighbors (r, c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

getCandidates :: Map Pos Char -> Pos -> Int -> Map Pos Int -> [Pos]
getCandidates grid pos steps minSteps = do
    npos <- neighbors pos
    guard (isJust (Map.lookup npos grid) &&
      getHeight (grid Map.! npos) <= getHeight (grid Map.! pos) + 1 &&
      maybe True (newSteps <) (Map.lookup npos minSteps))
    return npos
  where newSteps = steps + 1

updateQueue :: Map Pos Char -> Pos -> Int -> Map Pos Int -> Set PrioPos -> (Map Pos Int, Set PrioPos)
updateQueue grid pos steps minSteps pq =
    foldl' (\(ms, pq) npos -> (Map.insert npos newSteps ms, Set.insert (PrioPos (newSteps, npos)) pq))
      (minSteps, pq) candidates
  where
    candidates = getCandidates grid pos steps minSteps
    newSteps = steps + 1

findPath :: Map Pos Char -> Maybe Int
findPath grid = loop grid endPos (Set.fromList [PrioPos (0, pos) | pos <- starts]) (Map.fromList [(pos, 0) | pos <- starts])
  where
    starts = [pos | (pos@(r, c), ch) <- Map.toList grid, c == 0]
    endPos = head [pos | (pos, 'E') <- Map.toList grid]

loop :: Map Pos Char -> Pos -> Set PrioPos -> Map Pos Int -> Maybe Int
loop grid endPos pq minSteps = case minView pq of
  Nothing -> Nothing
  Just (PrioPos (steps, pos), rest) -> debug $ if pos == endPos then Just steps else loop grid endPos newPq newMinSteps
    where
      debug = trace ("Processing: pos " ++ show pos ++ " with " ++ show steps ++ " steps")
      (newMinSteps, newPq) = updateQueue grid pos steps minSteps rest

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = parseInput input
  case findPath grid of
    Just steps -> putStrLn ("Steps: " ++ show steps)
    Nothing -> putStrLn "No path found"
