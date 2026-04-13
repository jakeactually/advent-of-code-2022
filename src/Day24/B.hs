module Day24.B (main) where

import Control.Monad (guard)
import Data.Array (Array, (!), listArray)
import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

type Pt = (Int, Int)

data Dir = U | D | L | R deriving (Eq, Show)

type Blizzard = (Pt, Dir)

parseInput :: String -> (Int, Int, Pt, Pt, [Blizzard])
parseInput input = (height, width, start, goal, makeBlizzards rows)
  where
    rows = lines input
    (topRow, bottomRow) = (head rows, last rows)
    (height, width) = (length rows, length topRow)
    (Just startCol, Just goalCol) = (findOpenInRow topRow, findOpenInRow bottomRow)
    (start, goal) = ((0, startCol), (height - 1, goalCol))

makeBlizzards :: [String] -> [Blizzard]
makeBlizzards rows = do   
  (r, row) <- zip [0 ..] rows
  (c, ch) <- zip [0 ..] row
  guard (ch `elem` "^v<>")
  return ((r, c), toDir ch)

findOpenInRow :: (Num a, Enum a) => [Char] -> Maybe a
findOpenInRow row = case [c | (c, ch) <- zip [0 ..] row, ch == '.'] of
      [] -> Nothing
      (c : _) -> Just c

toDir :: Char -> Dir
toDir '^' = U
toDir 'v' = D
toDir '<' = L
toDir '>' = R
toDir _ = error "invalid blizzard direction"

advance :: (Int, Int) -> Blizzard -> Blizzard
advance (height, width) ((r, c), dir) = (nextPos, dir)
  where
    innerH = height - 2
    innerW = width - 2
    wrapRow x = ((x - 1) `mod` innerH) + 1
    wrapCol x = ((x - 1) `mod` innerW) + 1
    nextPos = case dir of
      U -> (wrapRow (r - 1), c)
      D -> (wrapRow (r + 1), c)
      L -> (r, wrapCol (c - 1))
      R -> (r, wrapCol (c + 1))

buildOccupiedCache :: (Int, Int) -> [Blizzard] -> Array Int (Set Pt)
buildOccupiedCache dims@(height, width) initial = listArray (0, cycleLen - 1) occupiedStates
  where
    cycleLen = lcm (height - 2) (width - 2)
    states = take cycleLen $ iterate (map (advance dims)) initial
    occupiedStates = map (Set.fromList . map fst) states

neighbors :: Pt -> [Pt]
neighbors (r, c) = [(r, c), (r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

isInsideInner :: (Int, Int) -> Pt -> Bool
isInsideInner (height, width) (r, c) = r >= 1 && r <= height - 2 && c >= 1 && c <= width - 2

-- | Minimum minutes to reach @legGoal@ from @legStart@, when the expedition may
-- begin moving at absolute minute @t0@ (blizzard state at minute @m@ uses @m \`mod\` cycleLen@).
shortestTimeFrom :: (Int, Int) -> Pt -> Pt -> Int -> Array Int (Set Pt) -> Int
shortestTimeFrom dims@(height, width) legStart legGoal t0 occupiedCache = bfs initialQueue initialSeen
  where
    cycleLen = lcm (height - 2) (width - 2)
    initialQueue = Seq.singleton (legStart, t0)
    initialSeen = Set.singleton (legStart, t0 `mod` cycleLen)

    isValid p = p == legStart || p == legGoal || isInsideInner dims p

    bfs :: Seq (Pt, Int) -> Set (Pt, Int) -> Int
    bfs Seq.Empty _ = error "no path found"
    bfs ((pos, t) :<| q) seen
      | pos == legGoal = t
      | otherwise = bfs nextQueue nextSeen
      where
        t' = t + 1
        blocked = occupiedCache ! (t' `mod` cycleLen)
        nextPositions = filter (\p -> isValid p && not (Set.member p blocked)) (neighbors pos)
        (nextQueue, nextSeen) = foldl' enqueue (q, seen) nextPositions
        enqueue (accQ, accSeen) p =
          let state = (p, t' `mod` cycleLen)
           in if Set.member state accSeen
                then (accQ, accSeen)
                else (accQ |> (p, t'), Set.insert state accSeen)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let (height, width, start, goal, blizzards) = parseInput content
  let dims = (height, width)
  let occupiedCache = buildOccupiedCache dims blizzards
  let t1 = shortestTimeFrom dims start goal 0 occupiedCache
  let t2 = shortestTimeFrom dims goal start t1 occupiedCache
  let t3 = shortestTimeFrom dims start goal t2 occupiedCache
  print t3
