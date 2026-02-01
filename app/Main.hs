module Main (main) where

import System.Directory (setCurrentDirectory)
import qualified Day01.A
import qualified Day01.B

main :: IO ()
main =
    setCurrentDirectory "src/Day01" >>
    Day01.A.main
