import Data.List (transpose)

row :: (Char, [Int]) -> Int
row ('*', xs) = product xs
row ('+', xs) = sum xs
row _ = error "unknown op"

findAnswer :: [(Char, [Int])] -> Int
findAnswer = sum . map row

parseCol :: [String] -> (Char, [Int])
parseCol [[x]] = (x, [])
parseCol (x : xs) = let (op, nums) = parseCol xs in (op, read x : nums)
parseCol [] = error "empty column"

main :: IO ()
main = do
  contents <- getContents
  let ls = map words $ lines contents
  let inp = map parseCol $ transpose ls
  print $ findAnswer inp
