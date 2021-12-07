import Data.List

part1 :: IO()
part1 = do
  input <- readFile "input/day7.txt"
  let parse = read $ "[" ++ input ++ "]" :: [Int]
  print $ fuel1 (median $ sort parse) parse

part2 :: IO()
part2 = do
  input <- readFile "input/day7.txt"
  let parse = read $ "[" ++ input ++ "]" :: [Int]
  print $ fuel2 (mean parse) parse

fuel1 :: Int -> [Int] -> Int
fuel1 m = foldl (\acc n -> acc + abs (n-m)) 0

fuel2 :: Int -> [Int] -> Int
fuel2 m ns = min (fuel m ns) (fuel (m+1) ns) where
    fuel m = foldl (\acc n -> acc + sum [0 .. abs (n-m)]) 0

median :: [Int] -> Int
median ns
  | even len  = div (ns !! ind + ns !! (ind+1)) 2
  | otherwise = ns !! (ind + 1) where
    len = length ns
    ind = div len 2 - 1

mean :: [Int] -> Int
mean ns = div (sum ns) (length ns)

