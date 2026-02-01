import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either (rights)
import Data.List (sort, elemIndex)

data Node = IntNode Int | ListNode [Node]
  deriving (Show, Eq)

instance Ord Node where
  compare (IntNode x) (IntNode y) = compare x y
  compare (ListNode (x : xs)) (ListNode (y : ys)) = case compare x y of
    EQ -> compare (ListNode xs) (ListNode ys)
    other -> other
  compare (ListNode xs) (ListNode ys) = compare (length xs) (length ys)
  compare (IntNode x) (ListNode ys) = compare (ListNode [IntNode x]) (ListNode ys)
  compare (ListNode xs) (IntNode y) = compare (ListNode xs) (ListNode [IntNode y])

parseInt :: Parser Node
parseInt = IntNode . read <$> many1 digit

parseList :: Parser Node
parseList = ListNode <$> between (char '[') (char ']') (parseNode `sepBy` char ',')

parseNode :: Parser Node
parseNode = try parseInt <|> parseList

parsePacket :: String -> Either ParseError Node
parsePacket = parse parseNode ""

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let inputLines = filter (not . null) (map (filter (/= '\r')) (lines contents))
  let parsed = rights $ map parsePacket inputLines
  let withDividers = ListNode [ListNode [IntNode 2]] : ListNode [ListNode [IntNode 6]] : parsed
  let sorted = sort withDividers
  let Just index1 = elemIndex (ListNode [ListNode [IntNode 2]]) sorted
  let Just index2 = elemIndex (ListNode [ListNode [IntNode 6]]) sorted
  print $ (index1 + 1) * (index2 + 1)
