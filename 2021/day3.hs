part1 :: IO ()
part1 = do
  input <- readFile "input/day3.txt"
  print $ product $ rates $ formatBits input where
    rates bs = map binToDec [pcRate gam bs, pcRate eps bs]

part2 :: IO ()
part2 = do
  input <- readFile "input/day3.txt"
  print $ product $ rates $ formatBits input where
    rates bs = map binToDec [genRate o2 bs, genRate co2 bs]

formatBits :: String -> [[Int]]
formatBits = map (map (\b -> read [b])) . lines

binToDec :: [Int] -> Int
binToDec = foldl (\acc b -> 2*acc + b) 0 

gam :: [Int] -> Int
gam = mcBit
o2 :: [Int] -> Int
o2  = mcBit
mcBit :: [Int] -> Int
mcBit bs
  | 2 * sum bs < length bs = 0
  | otherwise              = 1

eps :: [Int] -> Int
eps = lcBit
co2 :: [Int] -> Int
co2 = lcBit
lcBit :: [Int] -> Int
lcBit bs
  | 2 * sum bs < length bs = 1
  | otherwise              = 0

pcRate :: ([Int] -> Int) -> [[Int]] -> [Int]
pcRate _ ([]:_) = []
pcRate typ bits = mcb : pcRate typ (map (drop 1) bits)
    where  mcb  = typ $ map head bits

genRate :: ([Int] -> Int) -> [[Int]] -> [Int]
genRate typ = genRate' 0 where
  genRate' _ [b] = b
  genRate' i bs  = genRate' (i+1) (bitsCrit i bs)
  bitsCrit i bs  = filter (\b -> b!!i == typ (map (!!i) bs)) bs

