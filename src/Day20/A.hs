{-# LANGUAGE ScopedTypeVariables #-}

module Day20.A (main) where

import Control.Monad
import Control.Monad.ST
import qualified Data.IntMap as IM
import Data.STRef

data NodeData s a = NodeData
  { val :: a,
    left :: Node s a,
    right :: Node s a
  }

newtype Node s a = Node (STRef s (NodeData s a))

mkNodes :: [a] -> ST s [Node s a]
mkNodes xs = do
  nodesList <- mapM (\_ -> Node <$> newSTRef undefined) xs
  let nodes = IM.fromList (zip [0 ..] nodesList)
  let n = IM.size nodes

  forM_ (zip [0 ..] xs) $ \(i, v) -> do
    let (Node ref) = nodes IM.! i
    let l = nodes IM.! ((i - 1) `mod` n)
    let r = nodes IM.! ((i + 1) `mod` n)
    writeSTRef ref (NodeData v l r)

  return nodesList

toList :: Int -> Node s a -> ST s [a]
toList 0 _ = return []
toList n (Node ref) = do
  NodeData v _ r <- readSTRef ref
  rest <- toList (n - 1) r
  return (v : rest)

moveRight :: Node s a -> ST s ()
moveRight node@(Node refA) = do
  NodeData vA lNode@(Node refL) rNode@(Node refB) <- readSTRef refA
  NodeData vL llNode _ <- readSTRef refL
  NodeData vB _ rrNode@(Node refR) <- readSTRef refB
  NodeData vR _ rrrNode <- readSTRef refR

  writeSTRef refL (NodeData vL llNode rNode)
  writeSTRef refB (NodeData vB lNode node)
  writeSTRef refA (NodeData vA rNode rrNode)
  writeSTRef refR (NodeData vR node rrrNode)

moveLeft :: Node s a -> ST s ()
moveLeft (Node refA) = do
  NodeData _ lNode _ <- readSTRef refA
  moveRight lNode

solve :: [Int] -> [Int]
solve numbers = runST $ do
  nodes <- mkNodes numbers
  let n = length numbers
  case nodes of
    [] -> return []
    (headNode : _) -> do
      forM_ nodes $ \node@(Node ref) -> do
        NodeData v _ _ <- readSTRef ref
        if v < 0 then
          replicateM_ (abs v `rem` (n - 1)) (moveLeft node)
        else if v > 0 then
          replicateM_ (v `rem` (n - 1)) (moveRight node)
        else
          return ()
      toList n headNode

main :: IO ()
main = do
  file <- readFile "input.txt"
  let numbers = map read (lines file) :: [Int]
  print $ solve numbers
