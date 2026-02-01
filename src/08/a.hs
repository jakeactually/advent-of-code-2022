import System.IO
import Data.Array
import Data.Char (digitToInt)
import Control.Monad.State
import Data.Map
import qualified Debug.Trace as Debug
import Debug.Trace (traceShowM)

-- Function to read the file and parse it into a 2D array
readInputFile :: FilePath -> IO (Array (Int, Int) Int)
readInputFile filePath = do
    content <- readFile filePath
    let linesOfDigits = lines content
        rows = length linesOfDigits
        cols = length (head linesOfDigits)
        elements = [((r, c), digitToInt char) |
                    (r, line) <- zip [0..] linesOfDigits,
                    (c, char) <- zip [0..] line]
    return $ array ((0, 0), (rows - 1, cols - 1)) elements

data EvalState = EvalState { heights :: Map (Int, Int) Int, tallest :: Int, grid :: Array (Int, Int) Int, us :: Int }
  deriving Show

-- Helper function to update the state
updateState :: (Int, Int) -> State EvalState ()
updateState (r, c) = do
  st <- get
  let tree = grid st Data.Array.! (r, c)
  let ((r1, c1), (r2, c2)) = bounds (grid st)
  let isEdge = r == r1 || r == r2 || c == c1 || c == c2
  put $
    let addition = if tree > tallest st || isEdge then 1 else 0
    in st { heights = insertWith (+) (r, c) addition (heights st), tallest = max (tallest st) tree, us = us st + tree }

type Grid = Array (Int, Int) Int

reset :: State EvalState ()
reset = do
    st <- get
    put $ st { tallest = 0 }

-- Perform all traversals in sequence
performTraversals :: Grid -> State EvalState ()
performTraversals grid = traverseRowsLR grid >> traverseRowsRL grid >> traverseColsTB grid >> traverseColsBT grid
    where
        ((r1, c1), (r2, c2)) = bounds grid
        -- Traverse all rows left-to-right
        traverseRowsLR grid = do
            mapM_ (\r -> reset >> mapM_ (\c -> updateState (r, c)) [c1..c2]) [r1..r2]
        -- Traverse all rows right-to-left
        traverseRowsRL grid = do
            mapM_ (\r -> reset >> mapM_ (\c -> updateState (r, c)) (reverse [c1..c2])) [r1..r2]
        -- Traverse all columns top-to-bottom
        traverseColsTB grid = do
            mapM_ (\c -> reset >> mapM_ (\r -> updateState (r, c)) [r1..r2]) [c1..c2]
        -- Traverse all columns bottom-to-top
        traverseColsBT grid = do
            mapM_ (\c -> reset >> mapM_ (\r -> updateState (r, c)) (reverse [r1..r2])) [c1..c2]

main :: IO ()
main = do
    let filePath = "input.txt"
    int2DArray <- readInputFile filePath
    let initialState = EvalState empty 0 int2DArray 0
    -- Run the stateful traversal
    let finalState = execState (performTraversals int2DArray) initialState
    -- print $ heights finalState
    print $ length $ Prelude.filter ((> 0) . snd) $ toList $ heights finalState
