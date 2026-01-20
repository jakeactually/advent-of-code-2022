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

chunkBySizes :: [Int] -> [a] -> [[a]]
chunkBySizes _ [] = []
chunkBySizes (s:ss) xs =
  let (h,t) = splitAt s xs
  in if null h then [] else h : chunkBySizes ss t

computeGroupResults :: [Instr] -> [Int]
computeGroupResults instrs =
  let groups = chunkBySizes (20 : repeat 40) instrs
      sums = map (sum . map instrValue) groups
      accumulated = drop 1 $ scanl (+) 0 sums
      multipliers = iterate (+40) 20
  in zipWith (*) accumulated multipliers

main :: IO ()
main = do
  instrs <- parseFile "input.txt"
  let results = computeGroupResults (Addx2 1 : instrs)
  print $ sum $ take 6 results
