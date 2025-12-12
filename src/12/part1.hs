{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bifunctor (first, second)
import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

flipHor :: Set (Int, Int) -> Set (Int, Int)
flipHor = Set.map (second (2 -))

flipVer :: Set (Int, Int) -> Set (Int, Int)
flipVer = Set.map (first (2 -))

invert :: Set (Int, Int) -> Set (Int, Int)
invert = Set.map (\(i, j) -> (j, i))

rotateCw :: Set (Int, Int) -> Set (Int, Int)
rotateCw = flipHor . invert

variants :: Set (Int, Int) -> [Set (Int, Int)]
variants block = nub $ sort $ flippedHor ++ flippedVer ++ rotations
  where
    flippedHor = take 4 $ iterate rotateCw $ flipHor block
    flippedVer = take 4 $ iterate rotateCw $ flipVer block
    rotations = take 4 $ iterate rotateCw block

possibleOffsets :: (Int, Int) -> [(Int, Int)]
possibleOffsets (bi, bj) = [(oi, oj) | oi <- [0 .. bi - 3], oj <- [0 .. bj - 3]]

applyOffset :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
applyOffset (oi, oj) = Set.map (\(i, j) -> (i + oi, j + oj))

possiblePlacements :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> [Set (Int, Int)]
possiblePlacements (bi, bj) occupied block =
  nub $
    sort $
      [ Set.union occupied blockPoss
        | variant <- variants block,
          offset <- possibleOffsets (bi, bj),
          let blockPoss = applyOffset offset variant,
          Set.null $ Set.intersection occupied blockPoss
      ]

tryFitAll :: (Int, Int) -> Set (Int, Int) -> [Set (Int, Int)] -> Bool
tryFitAll (_, _) _ [] = True
tryFitAll (bi, bj) occupied blocks | sum (map Set.size blocks) > (bi * bj - Set.size occupied) = False
tryFitAll (bi, bj) occupied blocks | Set.null occupied && (bi `div` 3) * (bj `div` 3) >= length blocks = True
tryFitAll (bi, bj) occupied (b : bs) = or nexts
  where
    newOccupieds = possiblePlacements (bi, bj) occupied b
    nexts = [tryFitAll (bi, bj) newOccupied bs | newOccupied <- newOccupieds]

findAnswer :: [((Int, Int), [Int])] -> Map Int (Set (Int, Int)) -> Int
findAnswer regions blocks =
  sum
    [ if tryFitAll dims Set.empty fullBlocks then 1 else 0
      | (dims, bquants) <- regions,
        let bids = concatMap (\(i, q) -> replicate q i) (zip [0 ..] bquants),
        let fullBlocks = map (blocks Map.!) bids
    ]

parseRegion :: String -> ((Int, Int), [Int]) -- dimensions, block IDs
parseRegion xs = ((read w, read h), ids)
  where
    [rawDims, rawIds] = splitOn ": " xs
    [w, h] = splitOn "x" rawDims
    ids = map read $ splitOn " " rawIds

parseBlock :: [String] -> (Int, Set (Int, Int)) -- block ID,
parseBlock [] = error "invalid block"
parseBlock (x : xs) = (i, block)
  where
    i = read $ init x
    block = Set.fromList [(i, j) | (i, l) <- zip [0 ..] xs, (j, x) <- zip [0 ..] l, x == '#']

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let sections = splitOn [""] ls
  let blocks = Map.fromList $ map parseBlock $ init sections
  let regions = map parseRegion $ last sections
  print $ findAnswer regions blocks
