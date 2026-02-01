import Data.List (intersect)
import Data.Char (ord)
import qualified Data.Map as Map

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']

lettersMap :: Map.Map Char Integer
lettersMap = Map.fromList $ zip letters [1..]

getShared :: String -> Char
getShared line = head $ left `intersect` right
    where
        (left, right) = splitAt (length line `div` 2) line

main :: IO ()
main = do
    theLines <- lines <$> readFile "input.txt"
    let shared = map getShared theLines
    print $ sum $ map (lettersMap Map.!) shared
