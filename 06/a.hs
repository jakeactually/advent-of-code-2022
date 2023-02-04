main :: IO ()
main = do
    text <- readFile "input.txt"
    let (a : b : c : d : rest) = text
    print $ consume a b c d rest 0 0

consume :: Char -> Char -> Char -> Char -> String -> Int -> Int -> Int
consume a b c d rest i depth
    | depth == 4 = i
    | a /= b && a /= c && a /= d = continue $ depth + 1
    | otherwise = continue 0
        where
            e : newRest = rest
            continue = consume b c d e newRest $ i + 1
