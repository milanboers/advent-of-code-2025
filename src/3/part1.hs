import Data.Char (digitToInt)

largestJoltage :: Int -> [Int] -> Int
largestJoltage 1 xs = maximum xs
largestJoltage d xs = (10 ^ (d - 1)) * chosenDigit + largestJoltage (d - 1) rest
  where
    chosenDigit = maximum $ drop (d - 1) $ reverse xs
    rest = drop 1 $ dropWhile (/= chosenDigit) xs

findAnswer :: [[Int]] -> Int
findAnswer = sum . map (largestJoltage 2)

main :: IO ()
main = do
  contents <- getContents
  let banks = (map . map) digitToInt $ lines contents
  print $ findAnswer banks
