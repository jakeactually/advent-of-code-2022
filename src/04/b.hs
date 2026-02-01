import Text.Parsec ( char, digit, many, parse, Parsec )

type Range = (Int, Int)

main :: IO ()
main = do
    text <- readFile "input.txt"
    let info = map parseRanges $ lines text
    let contained = flip filter info $ \(a, b) -> a `overlaps` b || b `overlaps` a
    print $ length contained

overlaps :: Range -> Range -> Bool
overlaps (a1, b1) (a2, b2) = a1 <= a2 && b1 >= a2

parseRanges :: String -> (Range, Range)
parseRanges line = rs
    where
        Right rs = parse ranges "" line

number :: Parsec String () Int
number = read <$> many digit

tuple :: Parsec String () a -> Char -> Parsec String () (a, a)
tuple f sep = do
    a <- f
    char sep
    b <- f
    return (a, b)

range :: Parsec String () Range
range = tuple number '-'

ranges :: Parsec String () (Range, Range)
ranges = tuple range ','
