module Day22.B (main) where

import Control.Applicative ((<|>))
import Data.List (unsnoc)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Parsec (char, digit, many1, parse)
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
applyAction chart state (Forward n) = foldl (const . step chart) state [1 .. n]

dirDelta :: Dir -> (Int, Int)
dirDelta U = (0, -1)
dirDelta R = (1, 0)
dirDelta D = (0, 1)
dirDelta L = (-1, 0)

allCoordsInRow :: Map (Int, Int) Char -> Int -> [(Int, Int)]
allCoordsInRow chart y = [(x', y') | ((x', y'), _) <- Map.toList chart, y' == y]

allCoordsInCol :: Map (Int, Int) Char -> Int -> [(Int, Int)]
allCoordsInCol chart x = [(x', y') | ((x', y'), _) <- Map.toList chart, x' == x]

wrapPos :: Map (Int, Int) Char -> (Int, Int) -> Dir -> (Int, Int)
wrapPos chart (_, y) R = minimum $ allCoordsInRow chart y
wrapPos chart (_, y) L = maximum $ allCoordsInRow chart y
wrapPos chart (x, _) D = minimum $ allCoordsInCol chart x
wrapPos chart (x, _) U = maximum $ allCoordsInCol chart x

step :: Map (Int, Int) Char -> (Int, Int, Dir) -> (Int, Int, Dir)
step chart (x, y, dir) = fromMaybe (error "Should not happen") $ do
  let (dx, dy) = dirDelta dir
  let nextPos = (x + dx, y + dy)
  wrappedPos <- (nextPos <$ Map.lookup nextPos chart) <|> pure (wrapPos chart (x, y) dir)
  tile <- Map.lookup wrappedPos chart
  pure $ case tile of
    '.' -> (fst wrappedPos, snd wrappedPos, dir)
    '#' -> (x, y, dir)
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
