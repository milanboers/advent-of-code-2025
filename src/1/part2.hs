timesZero :: Int -> Int -> Int
timesZero 0 x = abs x `div` 100
timesZero c x | (c + x) <= 0 = abs (c + x) `div` 100 + 1
timesZero c x = (c + x) `div` 100

dial :: [Int] -> Int -> Int
dial (x : xs) c = timesZero c x + dial xs ((c + x) `mod` 100)
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
