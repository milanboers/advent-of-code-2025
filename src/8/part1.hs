import Data.Foldable (find)
import Data.List (foldl', sortBy, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set (Set)
import qualified Data.Set as Set

type Box = (Int, Int, Int)

dist :: Box -> Box -> Double
dist (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)

tuplify :: [Int] -> (Int, Int, Int)
tuplify [x, y, z] = (x, y, z)
tuplify _ = error "not of length 3"

connect :: Set (Set Box) -> (Box, Box) -> Set (Set Box)
connect circuits (b1, b2) = Set.insert newCircuit . Set.delete b1circuit . Set.delete b2circuit $ circuits
  where
    b1circuit = fromJust $ find (Set.member b1) circuits
    b2circuit = fromJust $ find (Set.member b2) circuits
    newCircuit = Set.union b1circuit b2circuit

findAnswer :: [Box] -> Int
findAnswer boxes = product $ take 3 $ sortBy (comparing Down) $ map length $ Set.toList connected
  where
    pairs = [(a, b) | a <- boxes, b <- boxes, a < b]
    circuits = Set.fromList $ map Set.singleton boxes
    queue = take 1000 $ sortOn (uncurry dist) pairs
    connected = foldl' connect circuits queue

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let boxes = map (tuplify . map read . splitOn ",") ls
  print $ findAnswer boxes
