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

step :: Set (Int, Int) -> Int
step papers = if removed == 0 then 0 else removed + step newPapers
  where
    newPapers = Set.filter (not . accessible papers) papers
    removed = Set.size papers - Set.size newPapers

findAnswer :: Set (Int, Int) -> Int
findAnswer = step

main :: IO ()
main = do
  contents <- getContents
  let papers = Set.fromList [(i, j) | (i, line) <- zip [1 ..] (lines contents), (j, x) <- zip [1 ..] line, x == '@']
  print $ findAnswer papers
