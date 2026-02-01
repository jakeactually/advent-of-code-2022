import Data.Text (pack, unpack, splitOn, strip)
import Data.List (drop, sort)

data Op = Add Int | Mul Int | Sq deriving Show

data Monkey = Monkey {
  monkeyIndex :: Int,
  items :: [Int],
  operation :: Op,
  testDiv :: Int,
  trueTarget :: Int,
  falseTarget :: Int,
  inspectionCount :: Int
} deriving Show

applyOp :: Op -> Int -> Int
applyOp (Add x) old = old + x
applyOp (Mul x) old = old * x
applyOp Sq old = old * old

parseMonkey :: String -> Monkey
parseMonkey text = Monkey index items op testDiv trueTarget falseTarget 0
  where
    (indexLine : itemsLine : opLine : testLine : trueLine : falseLine : _) = map unpack $ splitOn (pack "\n") (pack text)
    index = read $ takeWhile (/= ':') $ drop 7 indexLine  -- "Monkey 0:"
    itemsStr = drop 18 itemsLine  -- "  Starting items: "
    items = map read $ words $ map (\c -> if c == ',' then ' ' else c) itemsStr
    opStr = drop 19 opLine  -- "  Operation: new = "
    op = parseOp opStr
    testDiv = read $ drop 21 testLine  -- "  Test: divisible by "
    trueTarget = read $ drop 29 trueLine  -- "    If true: throw to monkey "
    falseTarget = read $ drop 30 falseLine  -- "    If false: throw to monkey "

parseOp :: String -> Op
parseOp s = case words s of
  ["old", "+", x] -> Add (read x)
  ["old", "*", "old"] -> Sq
  ["old", "*", x] -> Mul (read x)
  _ -> error $ "Invalid op: " ++ s

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

processItem :: Int -> Monkey -> Int -> (Int, Int)
processItem modVal m item = (target, newWorry)
  where newWorry = applyOp (operation m) item `mod` modVal
        target = if newWorry `mod` testDiv m == 0 then trueTarget m else falseTarget m

processMonkey :: Int -> [Monkey] -> Int -> [Monkey]
processMonkey modVal ms i = ms''
  where m = ms !! i
        processed = length (items m)
        throws = map (processItem modVal m) (items m)
        ms' = foldl (\acc (t, item) -> updateAt t (\m' -> m' { items = items m' ++ [item] }) acc) ms throws
        ms'' = updateAt i (\m' -> m' { items = [], inspectionCount = inspectionCount m' + processed }) ms'

simulateRound :: Int -> [Monkey] -> [Monkey]
simulateRound modVal monkeys = foldl (processMonkey modVal) monkeys [0..length monkeys - 1]

simulateRoundsIO :: Int -> Int -> [Monkey] -> IO [Monkey]
simulateRoundsIO _ 0 ms = return ms
simulateRoundsIO modVal n ms = do
  let newMs = simulateRound modVal ms
  putStrLn $ "After round " ++ show (10001 - n) ++ ":"
  mapM_ (\m -> putStrLn $ "Monkey " ++ show (monkeyIndex m) ++ ": " ++ show (inspectionCount m) ++ " inspections, items: " ++ show (items m)) newMs
  simulateRoundsIO modVal (n-1) newMs

main :: IO ()
main = do
  content <- readFile "input.txt"
  let chunks = splitOn (pack "\n\n") (pack content)
  let monkeys = map (parseMonkey . unpack . strip) chunks
  let divs = map testDiv monkeys
  let lcmDivs = foldl1 lcm divs
  print lcmDivs
  finalMonkeys <- simulateRoundsIO lcmDivs 10000 monkeys
  let counts = sort $ map inspectionCount finalMonkeys
  let answer = product $ take 2 $ reverse counts
  print answer
