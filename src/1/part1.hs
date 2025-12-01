dial :: [Int] -> Int -> Int
dial (x : xs) 0 = 1 + dial xs (x `mod` 100)
dial (x : xs) c = dial xs ((c + x) `mod` 100)
dial [] _ = 0

findAnswer :: [Int] -> Int
findAnswer nums = dial nums 50

parseLine :: String -> Int
parseLine ('L' : xs) = -(read xs)
parseLine ('R' : xs) = read xs
parseLine _ = error "invalid line"

main :: IO ()
main = do
  contents <- getContents
  let input = map parseLine $ lines contents
  print $ findAnswer input
