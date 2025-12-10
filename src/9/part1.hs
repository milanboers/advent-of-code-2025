import Data.List.Split (splitOn)

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

findAnswer :: [(Int, Int)] -> Int
findAnswer tiles = maximum [area t1 t2 | t1 <- tiles, t2 <- tiles, t1 < t2]

tuplify :: [Int] -> (Int, Int)
tuplify [x, y] = (x, y)
tuplify _ = error "not of length 2"

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let tiles = map (tuplify . map read . splitOn ",") ls
  print $ findAnswer tiles
