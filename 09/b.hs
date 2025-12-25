import System.IO
import qualified Data.Set as Set

data Direction = R | U | L | D deriving (Show, Read, Eq)

data Move = Move Direction Int deriving (Show, Eq)

type Pos = (Int, Int)

data State = State
    { knots   :: [Pos]  -- 10 knots: head is first, tail is last
    , visited :: Set.Set Pos
    } deriving (Show)

parseDirection :: Char -> Direction
parseDirection 'R' = R
parseDirection 'U' = U
parseDirection 'L' = L
parseDirection 'D' = D
parseDirection c   = error $ "Unknown direction: " ++ [c]

parseLine :: String -> Move
parseLine line = Move dir steps
  where
    dir   = parseDirection (head line)
    steps = read (drop 2 line) :: Int

parseInput :: String -> [Move]
parseInput = map parseLine . lines

-- Get the delta for a direction
dirDelta :: Direction -> Pos
dirDelta R = (1, 0)
dirDelta L = (-1, 0)
dirDelta U = (0, 1)
dirDelta D = (0, -1)

-- Add two positions
addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Check if tail is adjacent to head (including diagonal and same position)
isAdjacent :: Pos -> Pos -> Bool
isAdjacent (hx, hy) (tx, ty) = abs (hx - tx) <= 1 && abs (hy - ty) <= 1

-- Normalize a value to -1, 0, or 1
normalize :: Int -> Int
normalize n
    | n > 0     = 1
    | n < 0     = -1
    | otherwise = 0

-- Move follower towards leader (normalized step)
moveTowards :: Pos -> Pos -> Pos
moveTowards (lx, ly) (fx, fy) = (fx + dx, fy + dy)
  where
    dx = normalize (lx - fx)
    dy = normalize (ly - fy)

-- Update the chain of knots: given a moved leader, propagate through followers
propagateKnots :: [Pos] -> [Pos]
propagateKnots [] = []
propagateKnots [k] = [k]
propagateKnots (leader:follower:rest) = leader : propagateKnots (newFollower : rest)
  where
    newFollower = if isAdjacent leader follower
                  then follower
                  else moveTowards leader follower

-- Process a single step: move head, then propagate through all knots
stepOnce :: Direction -> State -> State
stepOnce dir state = State newKnots newVisited
  where
    oldKnots = knots state
    movedHead = addPos (head oldKnots) (dirDelta dir)
    newKnots = propagateKnots (movedHead : tail oldKnots)
    newVisited = Set.insert (last newKnots) (visited state)

-- Process a single move instruction (multiple steps)
processMove :: State -> Move -> State
processMove state (Move dir steps) = iterate (stepOnce dir) state !! steps

-- Process all moves
processMoves :: [Move] -> State
processMoves = foldl processMove initialState
  where
    initialKnots = replicate 10 (0, 0)  -- 10 knots all starting at origin
    initialState = State initialKnots (Set.singleton (0, 0))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let moves = parseInput contents
    let finalState = processMoves moves
    putStrLn $ "Tail visited " ++ show (Set.size (visited finalState)) ++ " positions"
