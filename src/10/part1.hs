import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Button = [Int]

type Machine = ([Bool], [Button], [Int]) -- diagram, buttons, joltages

pressButton :: [Bool] -> Button -> [Bool]
pressButton state button = zipWith (\i t -> (if i `elem` button then not t else t)) [0 ..] state

fewestPresses :: [Bool] -> [Button] -> Set [Bool] -> Set [Bool] -> Int
fewestPresses diagram _ _ currs | diagram `Set.member` currs = 0
fewestPresses diagram buttons seen currs = 1 + fewestPresses diagram buttons newSeen newCurrs
  where
    nexts = Set.unions [Set.map (`pressButton` b) currs | b <- buttons]
    newCurrs = Set.difference nexts seen
    newSeen = Set.union seen newCurrs

fewestPressesForMachine :: Machine -> Int
fewestPressesForMachine (diagram, buttons, _) =
  fewestPresses diagram buttons Set.empty $
    Set.singleton $
      replicate (length diagram) False

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
