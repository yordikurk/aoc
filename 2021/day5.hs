import qualified Data.Map as Map

type Point = (Int,Int)
type Line  = (Point,Point)
type Diag  = Map.Map Point Int

part :: Int -> IO()
part n = do
  input <- readFile "input/day5.txt"
  print . Map.size . Map.filter ( >1) . genMap n . parseInput $ input

parseInput :: String -> [Line]
parseInput = parse where
  parse  s = map (genL . filter (/= "->") . words) (lines s)
  genL   l = (genP $ head l, genP $ last l)
  genP   p = (read p1, read $ tail p2)
    where (p1,p2) = span (/= ',') p

points :: Int -> Line -> [Point]
points n ((x1,y1),(x2,y2))
  | x1 == x2  = [(x1,y) | y <- intv y1 y2]
  | y1 == y2  = [(x,y1) | x <- intv x1 x2]
  | otherwise = if n == 1 then [] else zip (intv x1 x2) (intv y1 y2)
    where intv a b | a > b = [a,a-1..b] | otherwise = [a..b] 

genMap :: Int -> [Line] -> Diag
genMap n = foldl addL Map.empty where
  addL diag l = foldl addP diag (points n l)
  addP diag p = Map.insert p (c+1) diag
    where c = Map.findWithDefault 0 p diag

