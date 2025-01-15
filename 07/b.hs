import Util
import qualified Data.Map as Map

-- Main function for testing
main :: IO ()
main = do
  let filePath = "input.txt"
  Right fileLines <- parseInputFile filePath
  let result = processLines fileLines
  let sums = sumFileSizes result
  print
    $ minimum
    $ filter (> (sums Map.! "/") - 40000000)
    $ map snd
    $ Map.toList sums
