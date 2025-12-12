{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad (replicateM)
import Data.List (elemIndex, foldl', (\\))
import Data.List.Split (splitOn)
import Data.Matrix (Matrix (nrows), extendTo, getCol, mapCol, ncols, rref, submatrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Vector as Vector

type Button = [Int]

type Machine = ([Bool], [Button], [Int]) -- diagram, buttons, joltages

buttonsForIndex :: [Button] -> Int -> [Bool]
buttonsForIndex buttons i = map (i `elem`) buttons

freeVariables :: Matrix Double -> [Int]
freeVariables m = [0 .. ncols m - 2] \\ catMaybes [fixedVar | row <- Matrix.toLists m, let fixedVar = elemIndex 1 row]

-- rref but works on tall matrices
betterRref :: Matrix Double -> Matrix Double
betterRref m = case rref (extendTo 0 (nrows m) (nrows m) m) of
  (Right mRref) -> submatrix 1 (nrows m) 1 (ncols m) mRref
  (Left x) -> error x

fixFreeVariable :: Matrix Double -> Int -> Int -> Matrix Double
fixFreeVariable m i v = mapCol (\rowi v' -> v' - (fromIntegral v * (col Vector.! (rowi - 1)))) (ncols m) m
  where
    col = getCol (i + 1) m

solveWithVars :: Matrix Double -> [(Int, Int)] -> Maybe Int
solveWithVars mRref freeVars =
  if all (\v -> let rv = fromIntegral $ round v in rv >= 0 && abs (v - rv) <= 1e-5) values
    then Just $ sum freeVarValues + round (sum values)
    else Nothing
  where
    freeVarValues = map snd freeVars
    endMatrix = foldl' (\m' (i, v) -> fixFreeVariable m' i v) mRref freeVars
    values = Vector.toList $ getCol (ncols endMatrix) endMatrix

solve :: Matrix Double -> Int
solve m = minimum $ mapMaybe (solveWithVars mRref) freeVarValues
  where
    mRref = betterRref m
    fvs = freeVariables mRref
    maxValue = ceiling $ Vector.maximum $ getCol (ncols m) m
    freeVarValues = map (zip fvs) (replicateM (length fvs) [0 .. maxValue])

fewestPressesForMachine :: Machine -> Int
fewestPressesForMachine (_, buttons, levels) =
  solve $
    Matrix.fromLists $
      zipWith (\i l -> map (fromIntegral . fromEnum) (buttonsForIndex buttons i) ++ [fromIntegral l]) [0 ..] levels

findAnswer :: [Machine] -> Int
findAnswer = sum . map fewestPressesForMachine

parseButton :: String -> Button
parseButton = map read . splitOn "," . init . drop 1

parseMachine :: String -> Machine
parseMachine xs = (diagram, buttons, joltages)
  where
    split = splitOn " " xs
    diagram = map (== '#') $ init . drop 1 $ head split
    buttons = map parseButton $ init . drop 1 $ split
    joltages = map read $ splitOn "," $ init . drop 1 $ last split

main :: IO ()
main = do
  contents <- getContents
  let machines = map parseMachine $ lines contents
  print $ findAnswer machines
