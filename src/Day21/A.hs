module Day21.A (main) where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr
  = Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var String
  | Val Int
  deriving (Show, Eq)

parseLine :: String -> (String, Expr)
parseLine line =
  case words line of
    (nameStr : rest) ->
      let name = init nameStr
       in case rest of
            [val] -> (name, Val (read val))
            [left, op, right] ->
              let leftExpr = Var left
                  rightExpr = Var right
                  expr = case op of
                    "+" -> Sum leftExpr rightExpr
                    "-" -> Sub leftExpr rightExpr
                    "*" -> Mul leftExpr rightExpr
                    "/" -> Div leftExpr rightExpr
                    _ -> error $ "Unknown operator: " ++ op
               in (name, expr)
            _ -> error "Invalid line"
    [] -> error "Empty line"

parseInput :: String -> Map String Expr
parseInput = Map.fromList . map parseLine . lines

main :: IO ()
main = do
  content <- readFile "input.txt"
  let parsedMap = parseInput content
  print $ Map.lookup "root" parsedMap
