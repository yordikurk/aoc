import qualified Data.Map as Map
import Data.Maybe
import Data.List

type Coding = Map.Map Char Char
-- example: [(a,g),(b,f),(c,e),(d,d),(e,c),(f,b),(g,a)]

part1 :: IO ()
part1 = do
  input <- readFile "input/day8.txt"
  let parse = parseOutput input
  print . countDigits . concat $ parse

part2 :: IO ()
part2 = do
  input <- readFile "input/day8.txt"
  let ins  = parseInput  input
  let outs = parseOutput input
  print . sum $ decode ins outs

parseInput :: String -> [[String]]
parseInput = map (words . takeWhile (/= '|')) . lines

parseOutput :: String -> [[String]]
parseOutput = map (words . tail . dropWhile (/= '|')) . lines

countDigits :: [String] -> Int
countDigits = length . filter (\s -> length s `elem` [2,3,4,7])

decode :: [[String]] -> [[String]] -> [Int]
decode ins outs = decimal where
  codings = zip (map searchCode ins) outs
  decodes = map (uncurry mapDigits) codings
  decimal = map (read . mapMaybe (`lookup` toDecimal)) decodes

validDigits :: [String]
validDigits = ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"]

toDecimal :: [(String,Char)]
toDecimal = [("abcefg",'0'),("cf",'1'),("acdeg",'2'),("acdfg",'3'),("bcdf",'4'),("abdfg",'5'),("abdefg",'6'),("acf",'7'),("abcdefg",'8'),("abcdfg",'9')]

allCodings :: [Coding]
allCodings = foldl coding [] (permutations "abcdefg") where
  coding acc perm = mkCoding perm : acc
  mkCoding perm =
    Map.insert 'a' (perm !! 0) $
    Map.insert 'b' (perm !! 1) $
    Map.insert 'c' (perm !! 2) $
    Map.insert 'd' (perm !! 3) $
    Map.insert 'e' (perm !! 4) $
    Map.insert 'f' (perm !! 5) $
    Map.insert 'g' (perm !! 6) Map.empty

mapDigits :: Coding -> [String] -> [String]
mapDigits code = map mapDigit where
  mapDigit = sort . mapMaybe (`Map.lookup` code)

checkCoding :: Coding -> [String] -> Bool
checkCoding code digits = all (`elem` validDigits) (mapDigits code digits)

searchCode :: [String] -> Coding
searchCode digits = testCodes digits allCodings where
  testCodes ds [] = error "No valid coding"
  testCodes ds (c:cs)
    | checkCoding c ds = c
    | otherwise        = testCodes ds cs
