import Data.Matrix

type Position = (Int,Int)

part1 :: IO ()
part1 = do
  input <- readFile "input/day11.txt"
  let m = fromLists [[read [n] :: Int | n <- ln] | ln <- lines input]
  print $ countFlashes 100 m

part2 :: IO ()
part2 = do
  input <- readFile "input/day11.txt"
  let m = fromLists [[read [n] :: Int | n <- ln] | ln <- lines input]
  print $ allFlash m

allFlash :: Matrix Int -> Int
allFlash = length . takeWhile (not . all (== 0)) . iterate (flash . step)

countFlashes :: Int -> Matrix Int -> Int
countFlashes n = sum . map (length . filterMatrix (== 0)) . take (n+1) . iterate (flash . step)

step :: Matrix Int -> Matrix Int
step = mapPos (\_ h -> h+1)

flash :: Matrix Int -> Matrix Int
flash m = if null flashPos then m else flash $ flash' m flashPos where
  flashPos = filterMatrix (> 9) m
  flash' :: Matrix Int -> [Position] -> Matrix Int
  flash' m pos = foldl doFlash m pos where
    doFlash :: Matrix Int -> Position -> Matrix Int
    doFlash m pos@(r,c) = foldl doFlash' m neighbors where
      doFlash' :: Matrix Int -> Position -> Matrix Int
      doFlash' m nbr
        | nbr == pos = setElem 0 pos m
        | m!nbr /= 0 = setElem (m!nbr + 1) nbr m
        | otherwise  = m
      neighbors :: [Position]
      neighbors = [(r + dr, c + dc) |
        dr <- [-1,0,1], r + dr > 0, r + dr <= nrows m,
        dc <- [-1,0,1], c + dc > 0, c + dc <= ncols m]

filterMatrix :: (Int -> Bool) -> Matrix Int -> [Position]
filterMatrix pred = map fst . filter (\(p,el) -> pred el) . toList . mapPos (\(r,c) el -> ((r,c),el))

