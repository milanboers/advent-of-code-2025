{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

allPathsFrom :: Map String [String] -> String -> Int
allPathsFrom _ "out" = 1
allPathsFrom cs from = sum $ map (allPathsFrom cs) (cs Map.! from)

findAnswer :: Map String [String] -> Int
findAnswer cs = allPathsFrom cs "you"

parseLine :: String -> (String, [String])
parseLine xs = let [from, rawTos] = splitOn ": " xs in (from, splitOn " " rawTos)

main :: IO ()
main = do
  contents <- getContents
  let ls = lines contents
  let cs = Map.fromList $ map parseLine ls
  print $ findAnswer cs
