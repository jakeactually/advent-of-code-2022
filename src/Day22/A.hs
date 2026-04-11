module Day22.A (main) where

import Data.List (unsnoc, foldl')
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Text.Parsec (char, digit, many1, parse, (<|>))
import Text.Parsec.String (Parser)

data Action = Forward Int | TurnLeft | TurnRight deriving (Show, Eq)

data Dir = R | D | L | U deriving (Show, Eq, Enum)

turnRight :: Dir -> Dir
turnRight d = toEnum ((fromEnum d + 1) `mod` 4)

turnLeft :: Dir -> Dir
turnLeft d = toEnum ((fromEnum d - 1) `mod` 4)

score :: (Int, Int, Dir) -> Int
score (x, y, dir) = 1000 * (y + 1) + 4 * (x + 1) + fromEnum dir

main :: IO ()
main = do
  file <- readFile "input.txt"
  let Just (chartLines, pathStr) = unsnoc $ lines file
  let chart = chartLinesToMap $ init chartLines
  let Right path = parse parsePath "" pathStr
  
  let startX = minimum [x | ((x, 0), _) <- Map.toList chart]
  let startState = (startX, 0, R)
  
  let finalState = foldl' (applyAction chart) startState path
  print finalState
  print (score finalState)

applyAction :: Map (Int, Int) Char -> (Int, Int, Dir) -> Action -> (Int, Int, Dir)
applyAction _ (x, y, dir) TurnLeft = (x, y, turnLeft dir)
applyAction _ (x, y, dir) TurnRight = (x, y, turnRight dir)
applyAction chart state (Forward n) = go n state
  where
    go 0 st = st
    go i st =
      let st'@(x', y', dir') = step chart st
      in if (x', y') == (fst3 st, snd3 st)
           then st
           else go (i - 1) st'
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b

step :: Map (Int, Int) Char -> (Int, Int, Dir) -> (Int, Int, Dir)
step chart (x, y, dir) =
  let (dx, dy) = case dir of
        U -> (0, -1)
        R -> (1, 0)
        D -> (0, 1)
        L -> (-1, 0)
      nextPos = (x + dx, y + dy)
      wrappedPos = case Map.lookup nextPos chart of
        Just _ -> nextPos
        Nothing -> case dir of
          R -> minimum [ (x', y) | ((x', y'), _) <- Map.toList chart, y' == y ]
          L -> maximum [ (x', y) | ((x', y'), _) <- Map.toList chart, y' == y ]
          D -> minimum [ (x, y') | ((x', y'), _) <- Map.toList chart, x' == x ]
          U -> maximum [ (x, y') | ((x', y'), _) <- Map.toList chart, x' == x ]
  in case Map.lookup wrappedPos chart of
       Just '.' -> (fst wrappedPos, snd wrappedPos, dir)
       Just '#' -> (x, y, dir)
       _ -> error "Should not happen"

parseAction :: Parser Action
parseAction =
  (Forward . read <$> many1 digit)
    <|> (TurnLeft <$ char 'L')
    <|> (TurnRight <$ char 'R')

parsePath :: Parser [Action]
parsePath = many1 parseAction

chartLinesToMap :: [String] -> Map (Int, Int) Char
chartLinesToMap chartLines = fromList $ do
  (y, chartLine) <- zip [0 ..] chartLines
  (x, chr) <- zip [0 ..] chartLine
  if chr == ' ' then [] else return ((x, y), chr)
