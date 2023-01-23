import Data.List ( transpose )
import Text.Parsec ( Parsec, digit, many, string, parse )
import Data.Either ( rights )

type Instruction = (Int, Int, Int)

split :: Int
split = 10

cols :: Int
cols = 9

main :: IO ()
main = do
    text <- readFile "input.txt"
    let stackText = take (split - 2) $ lines text
    let stack = map noSpaces $ transpose $ map extractLetters stackText
    let instructions = rights $ map (parse instruction "") $ drop split $ lines text
    let result = foldr apply ([] : stack) $ reverse instructions
    print $ map head $ drop 1 result

extractLetters :: [Char] -> [Char]
extractLetters line = map (line !!) $ take cols [1,5..]

noSpaces :: [Char] -> [Char]
noSpaces = filter (/= ' ')

number :: Parsec String () Int
number = read <$> many digit

instruction :: Parsec String () Instruction
instruction = do
    string "move "
    x <- number
    string " from "
    a <- number
    string " to "
    b <- number
    return (x, a, b)

apply :: Instruction -> [String] -> [String]
apply (n, a, b) stack = zipWith newColumn [0..] stack
    where
        columnA = stack !! a
        columnB = stack !! b
        (newColumnA, newColumnB) = (drop n columnA, take n columnA ++ columnB) 
        newColumn i column
            | i == a = newColumnA
            | i == b = newColumnB
            | otherwise = column
