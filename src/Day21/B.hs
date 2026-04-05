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

getValue :: Map String Expr -> String -> Int
getValue ctx key = eval (ctx Map.! key)
  where
    eval :: Expr -> Int
    eval (Val v) = v
    eval (Var k) = getValue ctx k
    eval (Sum a b) = eval a + eval b
    eval (Sub a b) = eval a - eval b
    eval (Mul a b) = eval a * eval b
    eval (Div a b) = eval a `div` eval b

getFullExpr :: Map String Expr -> String -> Expr
getFullExpr ctx key = eval (ctx Map.! key)
  where
    eval :: Expr -> Expr
    eval (Val v) = Val v
    eval (Var k) = getFullExpr ctx k
    eval (Sum a b) = Sum (eval a) (eval b)
    eval (Sub a b) = Sub (eval a) (eval b)
    eval (Mul a b) = Mul (eval a) (eval b)
    eval (Div a b) = Div (eval a) (eval b)
    eval Humn = Humn

prettyPrint :: Expr -> String
prettyPrint (Val v) = show v
prettyPrint (Var k) = k
prettyPrint (Sum a b) = "(" ++ prettyPrint a ++ " + " ++ prettyPrint b ++ ")"
prettyPrint (Sub a b) = "(" ++ prettyPrint a ++ " - " ++ prettyPrint b ++ ")"
prettyPrint (Mul a b) = "(" ++ prettyPrint a ++ " * " ++ prettyPrint b ++ ")"
prettyPrint (Div a b) = "(" ++ prettyPrint a ++ " / " ++ prettyPrint b ++ ")"
prettyPrint Humn = "x"

main :: IO ()
main = do
  content <- readFile "input.txt"
  let parsedMap = parseInput content
  -- putStrLn $ prettyPrint (getFullExpr parsedMap "root")
  print $ getValue parsedMap "vmqt"
