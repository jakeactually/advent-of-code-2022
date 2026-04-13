module Day23.A (main) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)

type Pt = (Int, Int)

parse :: String -> Set Pt
parse input = Set.fromList [ (r, c) | (r, row) <- zip [0..] (lines input), (c, char) <- zip [0..] row, char == '#' ]

data Dir = N | S | W | E deriving (Show, Eq)

neighbors :: Pt -> [Pt]
neighbors (r,c) = [ (r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], dr /= 0 || dc /= 0 ]

hasNeighbor :: Set Pt -> Pt -> Bool
hasNeighbor elves pt = any (`Set.member` elves) (neighbors pt)

getChecks :: Dir -> Pt -> [Pt]
getChecks N (r,c) = [(r-1, c), (r-1, c+1), (r-1, c-1)]
getChecks S (r,c) = [(r+1, c), (r+1, c+1), (r+1, c-1)]
getChecks W (r,c) = [(r, c-1), (r-1, c-1), (r+1, c-1)]
getChecks E (r,c) = [(r, c+1), (r-1, c+1), (r+1, c+1)]

getMove :: Dir -> Pt -> Pt
getMove N (r,c) = (r-1, c)
getMove S (r,c) = (r+1, c)
getMove W (r,c) = (r, c-1)
getMove E (r,c) = (r, c+1)

propose :: Set Pt -> [Dir] -> Pt -> Maybe Pt
propose elves dirs pt =
    if not (hasNeighbor elves pt)
    then Nothing
    else case filter (\d -> all (\p -> not (Set.member p elves)) (getChecks d pt)) dirs of
        [] -> Nothing
        (d:_) -> Just (getMove d pt)

step :: (Set Pt, [Dir]) -> (Set Pt, [Dir])
step (elves, dirs) = (nextElves, drop 1 dirs ++ take 1 dirs)
  where
    proposals :: [(Pt, Pt)]
    proposals = mapMaybe (\elf -> fmap (\dest -> (elf, dest)) (propose elves dirs elf)) (Set.toList elves)
    
    destCounts :: Map Pt Int
    destCounts = foldl' (\acc (_, dest) -> Map.insertWith (+) dest 1 acc) Map.empty proposals
    
    nextElves = foldl' move elves proposals
      where
        move acc (origin, dest)
            | Map.findWithDefault 0 dest destCounts == 1 = Set.insert dest (Set.delete origin acc)
            | otherwise = acc

runSteps :: Int -> (Set Pt, [Dir]) -> Set Pt
runSteps 0 (elves, _) = elves
runSteps n state = runSteps (n - 1) (step state)

countEmpty :: Set Pt -> Int
countEmpty elves =
    if Set.null elves then 0 else
    let xs = map snd (Set.toList elves)
        ys = map fst (Set.toList elves)
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        area = (maxX - minX + 1) * (maxY - minY + 1)
    in area - Set.size elves

main :: IO ()
main = do
  input <- readFile "input.txt"
  let elves = parse input
  let dirs = [N, S, W, E]
  let elves' = runSteps 10 (elves, dirs)
  print (countEmpty elves')
