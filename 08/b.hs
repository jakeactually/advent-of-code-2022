import Data.Array ( (!), array, bounds, Array )
import Data.Char (digitToInt)
import Data.List (transpose, maximumBy)
import qualified Data.Map as Map
import qualified Data.Array as Array

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

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

-- For a list of digits, compute the steps to the left until hitting a greater or equal digit
stepsToGreater :: [Int] -> [Int]
stepsToGreater xs = zipWith (curry go) [0..] xs
  where
    go (i, x) = length $ takeWhileInclusive (< x) $ reverse $ Prelude.take i xs

-- For a row, left-to-right
rowLR :: [Int] -> [Int]
rowLR = stepsToGreater

-- For a row, right-to-left
rowRL :: [Int] -> [Int]
rowRL = reverse . stepsToGreater . reverse

-- For a column, top-to-bottom
colTB :: [Int] -> [Int]
colTB = stepsToGreater

-- For a column, bottom-to-top
colBT :: [Int] -> [Int]
colBT = reverse . stepsToGreater . reverse

-- Apply a function to all rows of a 2D array
applyToRows :: ([[Int]] -> [[Int]]) -> Array (Int, Int) Int -> Array (Int, Int) Int
applyToRows f arr =
  let ((r1, c1), (r2, c2)) = bounds arr
      rows = [[arr Array.! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]
      newRows = f rows
      elems = [((r, c), newRows !! (r - r1) !! (c - c1)) | r <- [r1..r2], c <- [c1..c2]]
  in array ((r1, c1), (r2, c2)) elems

-- Apply a function to all columns of a 2D array
applyToCols :: ([[Int]] -> [[Int]]) -> Array (Int, Int) Int -> Array (Int, Int) Int
applyToCols f arr =
  let ((r1, c1), (r2, c2)) = bounds arr
      cols = [[arr Array.! (r, c) | r <- [r1..r2]] | c <- [c1..c2]]
      newCols = f cols
      newRows = transpose newCols
      elems = [((r, c), newRows !! (r - r1) !! (c - c1)) | r <- [r1..r2], c <- [c1..c2]]
  in array ((r1, c1), (r2, c2)) elems

main :: IO ()
main = do
    let filePath = "input.txt"
    int2DArray <- readInputFile filePath

    let ((r1, c1), (r2, c2)) = bounds int2DArray
        rows = [[int2DArray Array.! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]
        cols = transpose rows

        -- Four matrices
        matLR = applyToRows (Prelude.map rowLR) int2DArray
        matRL = applyToRows (Prelude.map rowRL) int2DArray
        matTB = applyToCols (Prelude.map colTB) int2DArray
        matBT = applyToCols (Prelude.map colBT) int2DArray

    putStrLn "Left-to-right:"
    mapM_ print [[matLR Array.! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]
    putStrLn "Right-to-left:"
    mapM_ print [[matRL Array.! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]
    putStrLn "Top-to-bottom:"
    mapM_ print [[matTB Array.! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]
    putStrLn "Bottom-to-top:"
    mapM_ print [[matBT Array.! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]

    -- Compute the product for each cell
    let products = [ ((r, c), matLR Array.! (r, c) * matRL Array.! (r, c) * matTB Array.! (r, c) * matBT Array.! (r, c))
                   | r <- [r1..r2], c <- [c1..c2] ]
        ((maxR, maxC), maxProd) = maximumBy (\(_, a) (_, b) -> compare a b) products

    putStrLn $ "Max product at (" ++ show maxR ++ ", " ++ show maxC ++ "): " ++ show maxProd
