import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

newBeam :: Set Int -> Int -> Set Int
newBeam splittersInRow bj | bj `Set.member` splittersInRow = Set.fromList [bj - 1, bj + 1]
newBeam _ bj = Set.singleton bj

row :: (Int, Int) -> Map Int (Set Int) -> Int -> (Set Int, Int)
row (si, sj) _ i | i == si = (Set.singleton sj, 0)
row s splitters i = (newBeams, Set.size activeSplitters + prevSplits)
  where
    (prevBeams, prevSplits) = row s splitters (i - 1)
    splittersInRow = Map.findWithDefault Set.empty i splitters
    activeSplitters = Set.intersection prevBeams splittersInRow
    newBeams = Set.unions $ Set.map (newBeam splittersInRow) prevBeams

findAnswer :: Int -> Map Int (Set Int) -> (Int, Int) -> Int
findAnswer h splitters s = let (_, splits) = row s splitters h in splits

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let height = length ls
  let elems = [(i, j, x) | (i, r) <- zip [1 ..] ls, (j, x) <- zip [1 ..] r]
  let splittersByRow = Map.fromListWith Set.union [(i, Set.singleton j) | (i, j, x) <- elems, x == '^']
  let start = head [(i, j) | (i, j, x) <- elems, x == 'S']
  print $ findAnswer height splittersByRow start
