module Main (main) where

import System.Directory (setCurrentDirectory)
import qualified Day14.A

main :: IO ()
main =
    setCurrentDirectory "src/Day14" >>
    Day14.A.main
