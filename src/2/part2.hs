import Data.List.Split (chunksOf, splitOn)

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x : y : ys) = x == y && allEqual (y : ys)

isInvalid :: Int -> Bool
isInvalid x = or [allEqual . chunksOf c $ show x | c <- [1 .. length (show x) `div` 2]]

findAnswer :: [(Int, Int)] -> Int
findAnswer xs =
  sum
    [ x
      | (i, j) <- xs,
        x <- [i .. j],
        isInvalid x
    ]

tuplify :: [Int] -> (Int, Int)
tuplify [x, y] = (x, y)
tuplify _ = error "must be of length 2"

main :: IO ()
main = do
  contents <- getContents
  let rawRanges = splitOn "," contents
  let ranges = map (tuplify . map read . splitOn "-") rawRanges
  print $ findAnswer ranges
