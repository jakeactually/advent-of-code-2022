import System.IO
import qualified Data.Set as Set

data Direction = R | U | L | D deriving (Show, Read, Eq)

data Move = Move Direction Int deriving (Show, Eq)

type Pos = (Int, Int)

data State = State
    { headPos  :: Pos
    , tailPos  :: Pos
    , visited  :: Set.Set Pos
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

-- Move tail towards head (normalized step)
moveTailTowards :: Pos -> Pos -> Pos
moveTailTowards (hx, hy) (tx, ty) = (tx + dx, ty + dy)
  where
    dx = normalize (hx - tx)
    dy = normalize (hy - ty)

-- Process a single step: move head, then possibly move tail
stepOnce :: Direction -> State -> State
stepOnce dir state = State newHead newTail newVisited
  where
    newHead = addPos (headPos state) (dirDelta dir)
    newTail = if isAdjacent newHead (tailPos state)
              then tailPos state
              else moveTailTowards newHead (tailPos state)
    newVisited = Set.insert newTail (visited state)

-- Process a single move instruction (multiple steps)
processMove :: State -> Move -> State
processMove state (Move dir steps) = iterate (stepOnce dir) state !! steps

-- Process all moves
processMoves :: [Move] -> State
processMoves = foldl processMove initialState
  where
    initialState = State (0, 0) (0, 0) (Set.singleton (0, 0))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let moves = parseInput contents
    let finalState = processMoves moves
    putStrLn $ "Tail visited " ++ show (Set.size (visited finalState)) ++ " positions"
