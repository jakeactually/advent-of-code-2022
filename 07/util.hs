module Util where

import System.IO
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Data.List
import Data.Maybe

-- Define the data type
data FileLine = Cd String | Ls | File String Int | Dir String deriving (Show, Eq)

-- Parser for a single line
lineParser :: Parser FileLine
lineParser = try cdParser <|> try lsParser <|> try dirParser <|> fileParser

cdParser :: Parser FileLine
cdParser = do
  string "$ cd "
  dir <- many1 (noneOf "\n")
  return $ Cd dir

lsParser :: Parser FileLine
lsParser = do
  string "$ ls"
  return Ls

dirParser :: Parser FileLine
dirParser = do
  string "dir "
  dirName <- many1 (noneOf "\n")
  return $ Dir dirName

fileParser :: Parser FileLine
fileParser = do
  size <- many1 digit
  char ' '
  name <- many1 (noneOf "\n")
  return $ File name (read size)

-- Parser for the entire file
fileParserAll :: Parser [FileLine]
fileParserAll = lineParser `endBy` newline

-- Function to parse the content of input.txt
parseInputFile :: FilePath -> IO (Either ParseError [FileLine])
parseInputFile filePath = do
  content <- readFile filePath
  return $ parse fileParserAll filePath content

-- Define the FileOrDir data type
data Entry
  = IsFile String Int
  | IsDir String
  deriving (Show, Eq)

-- Function to process the lines and return a Map
processLines :: [FileLine] -> Map.Map String [Entry]
processLines lines = fst $ foldl processLine (Map.empty, []) lines
  where
    -- Process each line, updating the map and current path
    processLine :: (Map.Map String [Entry], [String]) -> FileLine -> (Map.Map String [Entry], [String])
    processLine (acc, currentDir) (Cd dir) =
      case dir of
        ".." -> (acc, init currentDir) -- Move up one directory
        _    -> (acc, currentDir ++ [dir]) -- Move into a subdirectory
    processLine (acc, currentDir) Ls = (acc, currentDir) -- Listing doesn't affect the state
    processLine (acc, currentDir) (File name size) =
      let fullPath = makePath currentDir name
       in (Map.insertWith (++) fullPath [IsFile name size] acc, currentDir)
    processLine (acc, currentDir) (Dir name) =
      let fullPath = makePath currentDir name
       in (Map.insertWith (++) fullPath [IsDir name] acc, currentDir)

    -- Helper function to construct full path
    makePath :: [String] -> String -> String
    makePath dirs name = '/' : drop 2 (intercalate "/" dirs)

-- Recursive function to calculate the sum of file sizes in each directory
sumFileSizes :: Map.Map FilePath [Entry] -> Map.Map FilePath Int
sumFileSizes dirMap = Map.mapWithKey computeSize dirMap
  where
    computeSize :: FilePath -> [Entry] -> Int
    computeSize dir entries = sum (fileSizes ++ subdirSizes)
      where
        fileSizes = [size | IsFile _ size <- entries]
        subdirs = [subdir | IsDir subdir <- entries]
        base = if dir == "/" then "" else dir
        subdirSizes = mapMaybe (\subdir -> Map.lookup (base ++ "/" ++ subdir) dirSums) subdirs

    dirSums = Map.mapWithKey computeSize dirMap
