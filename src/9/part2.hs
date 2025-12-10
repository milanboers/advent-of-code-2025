import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

allColored :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
allColored coloredTiles (x1, y1) (x2, y2) =
  all (`Set.member` coloredTiles) [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

neighbors :: (Int, Int) -> (Int, Int) -> Set (Int, Int)
neighbors (maxX, maxY) (x, y) =
  Set.fromList
    [(nx, ny) | (nx, ny) <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)], nx >= 0, ny >= 0, nx <= maxX, ny <= maxY]

bfs :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
bfs _ seen curr | Set.null curr = seen
bfs bounds seen curr = bfs bounds nextSeen nextCurr
  where
    nexts = Set.unions $ Set.map (neighbors bounds) curr
    nextCurr = Set.difference nexts seen
    nextSeen = Set.union seen nexts

drawLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
drawLine p1@(x1, y1) p2@(x2, y2) =
  [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2], (x, y) /= p1, (x, y) /= p2]

drawLines :: [(Int, Int)] -> [(Int, Int)]
drawLines (x : y : ys) = drawLine x y ++ drawLines (y : ys)
drawLines _ = []

simplifyCoord :: [Int] -> Int -> Int
simplifyCoord (c : _) x | x == c = 1
simplifyCoord (c : _) x | x < c = 0
simplifyCoord (_ : cs) x = 2 + simplifyCoord cs x
simplifyCoord [] _ = 0

simplify :: [Int] -> [Int] -> (Int, Int) -> (Int, Int)
simplify xCoords yCoords (x, y) = (simplifyCoord xCoords x, simplifyCoord yCoords y)

findAnswer :: [(Int, Int)] -> Int
findAnswer originalRedTiles = maximum $ map (uncurry area) pairs
  where
    xCoords = nub $ sort [x | (x, _) <- originalRedTiles]
    yCoords = nub $ sort [y | (_, y) <- originalRedTiles]
    redTiles = map (simplify xCoords yCoords) originalRedTiles
    bounds@(maxX, maxY) = (1 + maximum (map fst redTiles), 1 + maximum (map snd redTiles))
    greenLines = Set.fromList $ drawLines $ last redTiles : redTiles
    outside = bfs bounds (Set.union (Set.fromList redTiles) greenLines) (Set.singleton (0, 0))
    inside = Set.difference (Set.fromList [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]) outside
    coloredTiles = Set.unions [greenLines, inside, Set.fromList redTiles]
    pairs =
      [ (t1, t2)
        | t1 <- originalRedTiles,
          t2 <- originalRedTiles,
          t1 < t2,
          let st1 = simplify xCoords yCoords t1,
          let st2 = simplify xCoords yCoords t2,
          allColored coloredTiles st1 st2
      ]

tuplify :: [Int] -> (Int, Int)
tuplify [x, y] = (x, y)
tuplify _ = error "not of length 2"

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let redTiles = map (tuplify . map read . splitOn ",") ls
  print $ findAnswer redTiles
