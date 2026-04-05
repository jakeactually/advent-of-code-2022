module Day21.B (main) where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr
  = Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var String
  | Val Int
  | Humn
  deriving (Show, Eq)

parseLine :: String -> (String, Expr)
parseLine line =
  case words line of
    ("humn:" : _) -> ("humn", Humn)
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


getFullExpr :: Map String Expr -> String -> Expr
getFullExpr ctx key = eval (ctx Map.! key)
  where
    eval :: Expr -> Expr
    eval (Val v) = Val v
    eval (Var k) = eval (ctx Map.! k)
    eval (Sum a b) = case (eval a, eval b) of
                       (Val va, Val vb) -> Val (va + vb)
                       (sa, sb) -> Sum sa sb
    eval (Sub a b) = case (eval a, eval b) of
                       (Val va, Val vb) -> Val (va - vb)
                       (sa, sb) -> Sub sa sb
    eval (Mul a b) = case (eval a, eval b) of
                       (Val va, Val vb) -> Val (va * vb)
                       (sa, sb) -> Mul sa sb
    eval (Div a b) = case (eval a, eval b) of
                       (Val va, Val vb) -> Val (va `div` vb)
                       (sa, sb) -> Div sa sb
    eval Humn = Humn

prettyPrint :: Expr -> String
prettyPrint (Val v) = show v
prettyPrint (Var k) = k
prettyPrint (Sum a b) = "(" ++ prettyPrint a ++ " + " ++ prettyPrint b ++ ")"
prettyPrint (Sub a b) = "(" ++ prettyPrint a ++ " - " ++ prettyPrint b ++ ")"
prettyPrint (Mul a b) = "(" ++ prettyPrint a ++ " * " ++ prettyPrint b ++ ")"
prettyPrint (Div a b) = "(" ++ prettyPrint a ++ " / " ++ prettyPrint b ++ ")"
prettyPrint Humn = "x"

solve :: Expr -> Int -> Int
solve Humn target = target
solve (Sum (Val v) e) target = solve e (target - v)
solve (Sum e (Val v)) target = solve e (target - v)
solve (Sub (Val v) e) target = solve e (v - target)
solve (Sub e (Val v)) target = solve e (target + v)
solve (Mul (Val v) e) target = solve e (target `div` v)
solve (Mul e (Val v)) target = solve e (target `div` v)
solve (Div (Val v) e) target = solve e (v `div` target)
solve (Div e (Val v)) target = solve e (target * v)
solve _ _ = error "unsolvable"

solveRoot :: Expr -> Int
solveRoot (Sum (Val target) e) = solve e target
solveRoot (Sum e (Val target)) = solve e target
solveRoot (Sub (Val target) e) = solve e target
solveRoot (Sub e (Val target)) = solve e target
solveRoot (Mul (Val target) e) = solve e target
solveRoot (Mul e (Val target)) = solve e target
solveRoot (Div (Val target) e) = solve e target
solveRoot (Div e (Val target)) = solve e target
solveRoot _ = error "not implemented or unsolvable"

main :: IO ()
main = do
  content <- readFile "input.txt"
  let parsedMap = parseInput content
  let rootExpr = getFullExpr parsedMap "root"
  putStrLn $ "Equation: " ++ prettyPrint rootExpr
  putStrLn $ "Part 2 Answer: " ++ show (solveRoot rootExpr)
