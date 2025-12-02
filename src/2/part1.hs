import Data.List.Split (splitOn)

halfRangeLow :: Int -> Int
halfRangeLow x | x < 10 = 1
halfRangeLow x = read . take (length (show x) `div` 2) $ show x

halfRangeHigh :: Int -> Int
halfRangeHigh x = read . take (ceiling $ fromIntegral (length (show x)) / 2) $ show x

findAnswer :: [(Int, Int)] -> Int
findAnswer xs =
  sum
    [ fullInvalid
      | (i, j) <- xs,
        halfInvalids <- [halfRangeLow i .. halfRangeHigh j],
        let fullInvalid = read . concat . replicate 2 . show $ halfInvalids,
        fullInvalid >= i,
        fullInvalid <= j
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
