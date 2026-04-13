module Main (main) where

import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import qualified Day01.A
import qualified Day01.B
import qualified Day02.A
import qualified Day02.B
import qualified Day03.A
import qualified Day03.B
import qualified Day04.A
import qualified Day04.B
import qualified Day05.A
import qualified Day05.B
import qualified Day06.A
import qualified Day06.B
import qualified Day07.A
import qualified Day07.B
import qualified Day08.A
import qualified Day08.B
import qualified Day09.A
import qualified Day09.B
import qualified Day10.A
import qualified Day10.B
import qualified Day11.A
import qualified Day11.B
import qualified Day12.A
import qualified Day12.B
import qualified Day13.A
import qualified Day13.B
import qualified Day14.A
import qualified Day14.B
import qualified Day15.A
import qualified Day15.B
import qualified Day16.A
import qualified Day16.B
import qualified Day17.A
import qualified Day17.B
import qualified Day18.A
import qualified Day18.B
import qualified Day19.A
import qualified Day19.B
import qualified Day20.A
import qualified Day20.B
import qualified Day21.A
import qualified Day21.B
import qualified Day22.A
import qualified Day22.B
import qualified Day23.A
import qualified Day23.B

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day, part] -> runDay day part
        _ -> putStrLn "Usage: stack run <day> <part> (e.g., 'stack run 1 a')"
    where
        runDay "1" "a" = setCurrentDirectory "src/Day01" >> Day01.A.main
        runDay "1" "b" = setCurrentDirectory "src/Day01" >> Day01.B.main
        runDay "2" "a" = setCurrentDirectory "src/Day02" >> Day02.A.main
        runDay "2" "b" = setCurrentDirectory "src/Day02" >> Day02.B.main
        runDay "3" "a" = setCurrentDirectory "src/Day03" >> Day03.A.main
        runDay "3" "b" = setCurrentDirectory "src/Day03" >> Day03.B.main
        runDay "4" "a" = setCurrentDirectory "src/Day04" >> Day04.A.main
        runDay "4" "b" = setCurrentDirectory "src/Day04" >> Day04.B.main
        runDay "5" "a" = setCurrentDirectory "src/Day05" >> Day05.A.main
        runDay "5" "b" = setCurrentDirectory "src/Day05" >> Day05.B.main
        runDay "6" "a" = setCurrentDirectory "src/Day06" >> Day06.A.main
        runDay "6" "b" = setCurrentDirectory "src/Day06" >> Day06.B.main
        runDay "7" "a" = setCurrentDirectory "src/Day07" >> Day07.A.main
        runDay "7" "b" = setCurrentDirectory "src/Day07" >> Day07.B.main
        runDay "8" "a" = setCurrentDirectory "src/Day08" >> Day08.A.main
        runDay "8" "b" = setCurrentDirectory "src/Day08" >> Day08.B.main
        runDay "9" "a" = setCurrentDirectory "src/Day09" >> Day09.A.main
        runDay "9" "b" = setCurrentDirectory "src/Day09" >> Day09.B.main
        runDay "10" "a" = setCurrentDirectory "src/Day10" >> Day10.A.main
        runDay "10" "b" = setCurrentDirectory "src/Day10" >> Day10.B.main
        runDay "11" "a" = setCurrentDirectory "src/Day11" >> Day11.A.main
        runDay "11" "b" = setCurrentDirectory "src/Day11" >> Day11.B.main
        runDay "12" "a" = setCurrentDirectory "src/Day12" >> Day12.A.main
        runDay "12" "b" = setCurrentDirectory "src/Day12" >> Day12.B.main
        runDay "13" "a" = setCurrentDirectory "src/Day13" >> Day13.A.main
        runDay "13" "b" = setCurrentDirectory "src/Day13" >> Day13.B.main
        runDay "14" "a" = setCurrentDirectory "src/Day14" >> Day14.A.main
        runDay "14" "b" = setCurrentDirectory "src/Day14" >> Day14.B.main
        runDay "15" "a" = setCurrentDirectory "src/Day15" >> Day15.A.main
        runDay "15" "b" = setCurrentDirectory "src/Day15" >> Day15.B.main
        runDay "16" "a" = setCurrentDirectory "src/Day16" >> Day16.A.main
        runDay "16" "b" = setCurrentDirectory "src/Day16" >> Day16.B.main
        runDay "17" "a" = setCurrentDirectory "src/Day17" >> Day17.A.main
        runDay "17" "b" = setCurrentDirectory "src/Day17" >> Day17.B.main
        runDay "18" "a" = setCurrentDirectory "src/Day18" >> Day18.A.main
        runDay "18" "b" = setCurrentDirectory "src/Day18" >> Day18.B.main
        runDay "19" "a" = setCurrentDirectory "src/Day19" >> Day19.A.main
        runDay "19" "b" = setCurrentDirectory "src/Day19" >> Day19.B.main
        runDay "20" "a" = setCurrentDirectory "src/Day20" >> Day20.A.main
        runDay "20" "b" = setCurrentDirectory "src/Day20" >> Day20.B.main
        runDay "21" "a" = setCurrentDirectory "src/Day21" >> Day21.A.main
        runDay "21" "b" = setCurrentDirectory "src/Day21" >> Day21.B.main
        runDay "22" "a" = setCurrentDirectory "src/Day22" >> Day22.A.main
        runDay "22" "b" = setCurrentDirectory "src/Day22" >> Day22.B.main
        runDay "23" "a" = setCurrentDirectory "src/Day23" >> Day23.A.main
        runDay "23" "b" = setCurrentDirectory "src/Day23" >> Day23.B.main
        runDay _ _ = putStrLn "Invalid day or part. Use 'stack run <day> <part>'"
