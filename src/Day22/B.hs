module Day22.B (main) where

import Control.Applicative ((<|>))
import Data.List (unsnoc)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
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

type Segment = ((Int, Int), (Int, Int))

type Rule = (Segment, Dir)

segmentsMap :: [(Rule, Rule)]
segmentsMap =
  [ ((((50, 0), (100, 0)), U), (((0, 150), (0, 200)), L)),
    ((((100, 0), (150, 0)), U), (((0, 200), (50, 200)), D)),
    ((((150, 0), (150, 50)), R), (((100, 150), (100, 100)), R)),
    ((((150, 50), (100, 50)), D), (((100, 100), (100, 50)), R)),
    ((((100, 150), (50, 150)), D), (((50, 200), (50, 150)), R)),
    ((((0, 150), (0, 100)), L), (((50, 0), (50, 50)), L)),
    ((((0, 100), (50, 100)), U), (((50, 50), (50, 100)), L))
  ]

allRules :: [(Rule, Rule)]
allRules = segmentsMap ++ map (\(a, b) -> (b, a)) segmentsMap

wrapCube :: (Int, Int) -> Dir -> ((Int, Int), Dir)
wrapCube nextPos dir =
  let matches =
        [ getDestCell t segDest dirDest
        | ((segSrc, dirSrc), (segDest, dirDest)) <- allRules,
          dirSrc == dir,
          Just t <- [getFaceExitT nextPos dir segSrc]
        ]
   in case matches of
        (res : _) -> res
        [] -> error ("No wrap rule found for " ++ show nextPos ++ " heading " ++ show dir)

getFaceExitT :: (Int, Int) -> Dir -> ((Int, Int), (Int, Int)) -> Maybe Int
getFaceExitT (nx, ny) dir ((x1, y1), (x2, y2)) =
  case dir of
    U | y1 == y2 && ny == y1 - 1 -> checkRange nx x1 x2
    D | y1 == y2 && ny == y1 -> checkRange nx x1 x2
    L | x1 == x2 && nx == x1 - 1 -> checkRange ny y1 y2
    R | x1 == x2 && nx == x1 -> checkRange ny y1 y2
    _ -> Nothing

checkRange :: Int -> Int -> Int -> Maybe Int
checkRange v a b
  | a < b = if a <= v && v < b then Just (v - a) else Nothing
  | otherwise = if b <= v && v < a then Just (a - 1 - v) else Nothing

getDestCell :: Int -> ((Int, Int), (Int, Int)) -> Dir -> ((Int, Int), Dir)
getDestCell t ((x1, y1), (x2, y2)) dirDest =
  let px = if x1 < x2 then x1 + t else if x1 > x2 then x1 - 1 - t else x1
      py = if y1 < y2 then y1 + t else if y1 > y2 then y1 - 1 - t else y1
      ox = if dirDest == R then 1 else 0
      oy = if dirDest == D then 1 else 0
   in ((px - ox, py - oy), inverseDir dirDest)

inverseDir :: Dir -> Dir
inverseDir U = D
inverseDir D = U
inverseDir L = R
inverseDir R = L

step :: Map (Int, Int) Char -> (Int, Int, Dir) -> (Int, Int, Dir)
step chart (x, y, dir) =
  let (dx, dy) = dirDelta dir
      nextPos = (x + dx, y + dy)
      (wrappedPos, newDir) = case Map.lookup nextPos chart of
        Just _ -> (nextPos, dir)
        Nothing -> wrapCube nextPos dir
   in case Map.lookup wrappedPos chart of
        Just '.' -> (fst wrappedPos, snd wrappedPos, newDir)
        Just '#' -> (x, y, dir)
        _ -> error "Should not happen (wrapped out of bounds)"

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
