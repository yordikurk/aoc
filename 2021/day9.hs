import qualified Data.Map as Map
import Data.Matrix
import Data.Maybe
import Data.List

type Point = (Int,Int)
type Basin = [Point]

part1 :: IO ()
part1 = do
  input <- readFile "input/day9.txt"
  let mtrx = fromLists [[read [c] :: Int | c <- r] | r <- lines input]
  print . sum . riskLevels $ mtrx

part2 :: IO ()
part2 = do
  input <- readFile "input/day9.txt"
  let mtrx = fromLists [[read [c] :: Int | c <- r] | r <- lines input]
  print . product . take 3 . sortBy (flip compare) . map length . basins $ mtrx

nbrsPos :: Point -> [Point]
nbrsPos (r,c) = [(r-1,c),(r,c+1),(r+1,c),(r,c-1)]

neighbors :: Point -> Matrix Int -> [Point]
neighbors (r,c) mtrx = filter inMatrix nbrsPos where
  inMatrix (r,c) = r > 0 && c > 0 && r <= nrows mtrx && c <= ncols mtrx
  nbrsPos = [(r-1,c),(r,c+1),(r+1,c),(r,c-1)]

lowPoints :: Matrix Int -> [Point]
lowPoints mtrx = filter (/= (0,0)) . toList $ mapPos lowPoint mtrx where
  lowPoint (r,c) h = if all (h<) nbrs then (r,c) else (0,0) where
    nbrs = catMaybes [safeGet r' c' mtrx | (r',c') <- nbrsPos (r,c)]

riskLevels :: Matrix Int -> [Int]
riskLevels mtrx = map (\lp -> mtrx ! lp + 1) (lowPoints mtrx)

basins :: Matrix Int -> [Basin]
basins mtrx = map (`basin` mtrx) (lowPoints mtrx)

basin :: Point -> Matrix Int -> Basin
basin point matrix = basin' point matrix [] where
  basin' p mtrx vis = foldl check vis (neighbors p mtrx) where
    check vis' np | np `elem` vis' || mtrx ! np == 9 = vis'
                  | otherwise = basin' np mtrx (np:vis')

