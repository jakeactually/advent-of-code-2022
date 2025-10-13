import Util
import qualified Data.Map as Map

-- Main function for testing
main :: IO ()
main = do
  let filePath = "input.txt"
  Right fileLines <- parseInputFile filePath
  let result = processLines fileLines
  print
    $ sum
    $ filter (<= 100000)
    $ map snd
    $ Map.toList
    $ sumFileSizes result
