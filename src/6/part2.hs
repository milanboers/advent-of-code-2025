import Data.List (transpose)
import Data.List.Split (splitWhen)

row :: (Char, [Int]) -> Int
row ('*', xs) = product xs
row ('+', xs) = sum xs
row _ = error "unknown op"

findAnswer :: [(Char, [Int])] -> Int
findAnswer = sum . map row

parseBlock :: [String] -> (Char, [Int])
parseBlock (x : xs) = case last x of
  ' ' -> let (op, ns) = parseBlock xs in (op, n : ns)
  op -> (op, [n])
  where
    n = read $ init x
parseBlock [] = error "empty block"

main :: IO ()
main = do
  contents <- getContents
  let cs = transpose $ lines contents
  let rawBlocks = map reverse $ splitWhen (all (== ' ')) cs
  let blocks = map parseBlock rawBlocks
  print $ findAnswer blocks
