module Day15.A where

import Text.Regex.PCRE
import Data.Maybe (mapMaybe)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sensors = sensorsAndBeacons input
    let results = mapMaybe calculateLineRange sensors
    print results

calculateLineRange :: ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
calculateLineRange ((sx, sy), (bx, by))
    | lineLength < 0 = Nothing
    | otherwise = Just (lineStart, lineEnd)
    where
        distToBeacon = manhattan (sx, sy) (bx, by)
        distToAnchor = manhattan (sx, sy) (sx, 10)
        lineLength = distToBeacon - distToAnchor
        lineStart = sx - lineLength
        lineEnd = sx + lineLength

sensorsAndBeacons :: String -> [((Int, Int), (Int, Int))]
sensorsAndBeacons = map (listTo2Pairs . map read . getAllTextMatches . (=~ "[0-9]+")) . lines
    where
        listTo2Pairs [sx, sy, bx, by] = ((sx, sy), (bx, by))
        listTo2Pairs _ = error "Unexpected number of elements"
