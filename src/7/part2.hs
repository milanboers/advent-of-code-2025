import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

newBeam :: Set Int -> Int -> [Int]
newBeam splittersInRow bj | bj `Set.member` splittersInRow = [bj - 1, bj + 1]
newBeam _ bj = [bj]

row :: (Int, Int) -> Map Int (Set Int) -> Int -> Map Int Int
row (si, sj) _ i | i == si = Map.singleton sj 1
row s splitters i = Map.fromListWith (+) [(nj, c) | (j, c) <- Map.toList prevBeams, nj <- newBeam splittersInRow j]
  where
    prevBeams = row s splitters (i - 1)
    splittersInRow = Map.findWithDefault Set.empty i splitters

findAnswer :: Int -> Map Int (Set Int) -> (Int, Int) -> Int
findAnswer h splitters s = sum $ Map.elems $ row s splitters h

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let height = length ls
  let elems = [(i, j, x) | (i, r) <- zip [1 ..] ls, (j, x) <- zip [1 ..] r]
  let splittersByRow = Map.fromListWith Set.union [(i, Set.singleton j) | (i, j, x) <- elems, x == '^']
  let start = head [(i, j) | (i, j, x) <- elems, x == 'S']
  print $ findAnswer height splittersByRow start
