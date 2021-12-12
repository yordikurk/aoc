import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe

type Cave    = String
type Path    = [Cave]
type Network = [(Cave,[Cave])]

part1 :: IO ()
part1 = do
  input <- readFile "input/day12.txt"
  print . length . paths legal1 . createNet . map (splitOn "-") . lines $ input

part2 :: IO ()
part2 = do
  input <- readFile "input/day12.txt"
  print . length . paths legal2 . createNet . map (splitOn "-") . lines $ input

createNet :: [[String]] -> Network
createNet input = map create allCaves where
  allCaves         = nub $ concat input
  create cave      = (cave, connections cave)
  connections cave = filter (/= cave) . concat . filter (elem cave) $ input

paths :: (Path -> Bool) -> Network -> [Path]
paths legal net = pathing [["start"]] where
  pathing paths
    | all (\(p:ps) -> p == "end") paths = paths
    | otherwise = pathing $ filter legal stepped
    where stepped = concatMap (travel net) paths

travel :: Network -> Path -> [Path]
travel net [] = error "Empty path"
travel net path@(currentCave:_)
  | currentCave == "end" = [path]
  | otherwise = map (: path) (connections currentCave)
  where connections cave = fromMaybe (error "Not a cave") (lookup cave net)

legal1 :: Path -> Bool
legal1 path = smallCaves == nub smallCaves where
  smallCaves = filter (isLower . head) path

legal2 :: Path -> Bool
legal2 path = length diff <= 1 && diff /= ["start"] && diff /= ["end"] where
  diff = smallCaves \\ nub smallCaves
  smallCaves = filter (isLower . head) path

