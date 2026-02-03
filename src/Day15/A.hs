module Day15.A where

import Text.Regex.PCRE
import Data.Maybe (mapMaybe)
import Data.List (sortOn)

data Limit = Start Int | End Int
    deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sensors = sensorsAndBeacons input
    let results = mapMaybe calculateLineRange sensors
    let limits = sortOn limitToInt $ concatMap tupleToLimit results
    print limits
    print $ sumDistancesWrapper limits

sensorsAndBeacons :: String -> [((Int, Int), (Int, Int))]
sensorsAndBeacons = map (listTo2Pairs . map read . getAllTextMatches . (=~ "-?[0-9]+")) . lines
    where
        listTo2Pairs [sx, sy, bx, by] = ((sx, sy), (bx, by))
        listTo2Pairs _ = error "Unexpected number of elements"

calculateLineRange :: ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
calculateLineRange ((sx, sy), (bx, by))
    | lineLength < 0 = Nothing
    | otherwise = Just (lineStart, lineEnd)
    where
        distToBeacon = manhattan (sx, sy) (bx, by)
        distToAnchor = manhattan (sx, sy) (sx, 2000000)
        lineLength = distToBeacon - distToAnchor
        lineStart = sx - lineLength
        lineEnd = sx + lineLength

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

tupleToLimit :: (Int, Int) -> [Limit]
tupleToLimit (start, end) = [Start start, End end]

limitToInt :: Limit -> Int
limitToInt (Start x) = x
limitToInt (End x) = x

limitToBinary :: Limit -> Int
limitToBinary (Start _) = 1
limitToBinary (End _) = -1

sumDistances :: [Limit] -> Int -> Int
sumDistances (a:b:rest) opens = addition + sumDistances (b:rest) (opens + limitToBinary b)
    where
        distance = limitToInt b - limitToInt a
        addition = if opens > 0 then distance else 0
sumDistances _ _ = 0

sumDistancesWrapper :: [Limit] -> Int
sumDistancesWrapper limits = sumDistances limits $ limitToBinary (head limits)
