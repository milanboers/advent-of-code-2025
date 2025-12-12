{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Function (fix)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MemoUgly (memo)

allPathsBetween :: Map String [String] -> String -> (String -> Int) -> String -> Int
allPathsBetween _ to _ from | from == to = 1
allPathsBetween cs _ f from = sum $ map f $ Map.findWithDefault [] from cs

allPathsFromToMemo :: Map String [String] -> String -> String -> Int
allPathsFromToMemo cs to = fix (memo . allPathsBetween cs to)

findAnswer :: Map String [String] -> Int
findAnswer cs =
  allPathsFromToMemo cs "fft" "svr"
    * allPathsFromToMemo cs "dac" "fft"
    * allPathsFromToMemo cs "out" "dac"
    + allPathsFromToMemo cs "dac" "svr"
      * allPathsFromToMemo cs "fft" "dac"
      * allPathsFromToMemo cs "out" "fft"

parseLine :: String -> (String, [String])
parseLine xs = let [from, rawTos] = splitOn ": " xs in (from, splitOn " " rawTos)

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let cs = Map.fromList $ map parseLine ls
  print $ findAnswer cs
