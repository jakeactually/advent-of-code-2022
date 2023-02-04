main :: IO ()
main = do
    text <- readFile "input.txt"
    let (a : rest) = text
    print $ consume a rest 0 0

consume :: Char -> String -> Int -> Int -> Int
consume a rest index depth
    | depth == 14 = index
    | a `notElem` take (13 - depth) rest = continue $ depth + 1
    | otherwise = continue 0
        where
            b : newRest = rest
            continue = consume b newRest $ index + 1
