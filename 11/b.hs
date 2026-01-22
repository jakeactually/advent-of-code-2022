import System.IO (readFile)
import Data.List (unfoldr)

data Instr = Noop | Addx1 | Addx2 Int deriving (Show, Eq)

parseLine :: String -> [Instr]
parseLine s = case words s of
  ["noop"] -> [Noop]
  ["addx", n] -> [Addx1, Addx2 (read n)]
  _ -> error ("Invalid instruction: " ++ s)

parse :: String -> [Instr]
parse = concatMap parseLine . lines

parseFile :: FilePath -> IO [Instr]
parseFile path = parse <$> readFile path

instrValue :: Instr -> Int
instrValue Noop      = 0
instrValue Addx1     = 0
instrValue (Addx2 n) = n

evaluate :: [Instr] -> [Bool]
evaluate instrs = go 0 1 instrs []
  where
    go :: Int -> Int -> [Instr] -> [Bool] -> [Bool]
    go _ _ [] acc = reverse acc
    go idx x (i:is) acc =
      let midx = idx `mod` 40
          flag = abs (x - midx) <= 1
          x' = x + instrValue i
      in go (idx + 1) x' is (flag : acc)

chunk :: Int -> [a] -> [[a]]
chunk n = takeWhile (not . null) . unfoldr (Just . splitAt n)

main :: IO ()
main = do
  instrs <- parseFile "input.txt"
  let pixels = evaluate instrs
      rows = chunk 40 pixels
  mapM_ (putStrLn . map (\b -> if b then '#' else '.')) rows
