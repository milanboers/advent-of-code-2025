{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges ing = any (\(l, h) -> l <= ing && ing <= h) ranges

findAnswer :: [(Int, Int)] -> [Int] -> Int
findAnswer ranges = length . filter (isFresh ranges)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "must be of length 2"

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let [rawRanges, rawIngredients] = splitOn [""] ls
  let ranges = map (tuplify . map read . splitOn "-") rawRanges
  let ingedients = map read rawIngredients
  print $ findAnswer ranges ingedients
