import Data.Set (Set)
import qualified Data.Set as Set

accessible :: Set (Int, Int) -> (Int, Int) -> Bool
accessible papers (i, j) =
  length
    [ (ai, aj)
      | ai <- [i - 1 .. i + 1],
        aj <- [j - 1 .. j + 1],
        (ai, aj) /= (i, j),
        (ai, aj) `Set.member` papers
    ]
    < 4

findAnswer :: Set (Int, Int) -> Int
findAnswer papers = Set.size $ Set.filter (accessible papers) papers

main :: IO ()
main = do
  contents <- getContents
  let papers = Set.fromList [(i, j) | (i, line) <- zip [1 ..] (lines contents), (j, x) <- zip [1 ..] line, x == '@']
  print $ findAnswer papers
