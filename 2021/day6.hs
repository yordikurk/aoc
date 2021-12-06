import Data.List

part1 :: IO()
part1 = do
  input <- readFile "input/day6.txt"
  print . length . (!!80) . iterate model1 . read $ "[" ++ input ++ "]"

model1 :: [Int] -> [Int]
model1 = foldr (\n ns -> if n==0 then [6,8]++ns else (n-1):ns) []

type Dictionary = [(Int,Int)]

part2 :: IO()
part2 = do
  input <- readFile "input/day6.txt"
  let parsed   = group $ sort (read ("[" ++ input ++ "]") :: [Int])
  let initDict = foldl (\d k -> raiseVals [(head k, length k)] d) [(k,0) | k <- [0..9]] parsed
  let dict     = iterate model2 initDict !! 256
  print $ sum [v | (k,v) <- dict]

model2 :: Dictionary -> Dictionary
model2 dict = foldl cycleKey dict dict where
  cycleKey dict (k,v)
    | k == 0    = raiseVals [(0,-v),(6,v),(8,v)] dict
    | otherwise = raiseVals [(k,-v),(k-1,v)]     dict

raiseVals :: [(Int, Int)] -> Dictionary -> Dictionary
raiseVals ks dict = foldl (flip raiseVal) dict ks where
  raiseVal (k,r) = map (\(k',v) -> if k==k' then (k,v+r) else (k',v))

