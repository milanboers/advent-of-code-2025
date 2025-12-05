{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (sort)
import Data.List.Split (splitOn)

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ((xl, xh) : (yl, yh) : ys) | xh + 1 >= yl = mergeRanges ((xl, max xh yh) : ys)
mergeRanges (x : y : ys) = x : mergeRanges (y : ys)
mergeRanges xs = xs

countIds :: [(Int, Int)] -> Int
countIds ((l, h) : xs) = h - l + 1 + countIds xs
countIds [] = 0

findAnswer :: [(Int, Int)] -> Int
findAnswer = countIds . mergeRanges . sort

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
tuplify _ = error "must be of length 2"

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let [rawRanges, _] = splitOn [""] ls
  let ranges = map (tuplify . map read . splitOn "-") rawRanges
  print $ findAnswer ranges
