part1 :: IO ()
part1 = do
  input <- readFile "input/day1.txt"
  print $ numsIncr $ map read (lines input)

numsIncr :: [Int] -> Int 
numsIncr []  = 0
numsIncr [_] = 0
numsIncr (x:y:zs)
  | y > x     = 1 + numsIncr (y:zs)
  | otherwise =     numsIncr (y:zs)

part2 :: IO ()
part2 = do
  input <- readFile "input/day1.txt"
  print $ windowsIncr $ map read (lines input)

sum3Nums :: [Int] -> [Int]
sum3Nums []         = []
sum3Nums [_]        = []
sum3Nums [_,_]      = []
sum3Nums (x:y:z:zs) = x+y+z : sum3Nums (y:z:zs)

windowsIncr :: [Int] -> Int
windowsIncr = numsIncr . sum3Nums

