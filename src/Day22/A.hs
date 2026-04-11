module Day22.A (main) where

import Data.List (unsnoc)
import Data.Map (Map, fromList)
import Text.Parsec (char, digit, many1, parse, (<|>))
import Text.Parsec.String (Parser)

data Action = Forward Int | TurnLeft | TurnRight deriving (Show, Eq)

main :: IO ()
main = do
  file <- readFile "input.txt"
  let Just (chartLines, pathStr) = unsnoc $ lines file
  let chart = chartLinesToMap $ init chartLines
  let Right path = parse parsePath "" pathStr
  print path

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
  return ((x, y), chr)
