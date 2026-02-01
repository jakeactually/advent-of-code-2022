import Data.List (intersect)
import Data.Char (ord)
import qualified Data.Map as Map

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']

lettersMap :: Map.Map Char Integer
lettersMap = Map.fromList $ zip letters [1..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = left : chunksOf n right
    where
        (left, right) = splitAt n xs

main :: IO ()
main = do
    theLines <- lines <$> readFile "input.txt"
    let badges = map (head . foldr1 intersect) $ chunksOf 3 theLines
    print $ sum $ map (lettersMap Map.!) badges
