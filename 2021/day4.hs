part1 :: IO ()
part1 = do
  input <- readFile "input/day4.txt"
  let (nums,brds) = parseParam . parseInput . lines $ input
  print $ play1 nums brds

part2 :: IO ()
part2 = do
  input <- readFile "input/day4.txt"
  let (nums,brds) = parseParam . parseInput . lines $ input
  print $ play2 nums brds

parseParam :: [[String]] -> ([Int],[Board])
parseParam params = (nums,brds) where
  nums  = read $ "[" ++ concat (head params) ++ "]"
  brds  = map brd (tail params)
  brd b = [map (\n -> C (read n) False) (words r) | r <- b]

parseInput :: [String] -> [[String]]
parseInput brds
  | null bs   = [b]
  | otherwise = b : parseInput (tail bs)
  where (b,bs)  = span (/= "") brds

type Board = [[Cell]]
data Cell  = C Int Bool deriving (Show)
instance Eq Cell where (C n1 _) == (C n2 _) = n1 == n2
nC :: Cell -> Int
nC (C n _) = n
bC :: Cell -> Bool
bC (C _ b) = b

play1 :: [Int] -> [Board] -> Int
play1 [] _ = error "Nobody wins"
play1 (n:ns) brds
  | null winners = play1 ns newBrds
  | otherwise    = n * (sum . noMarkC . head $ winners)
  where
    winners = wins newBrds
    newBrds = map (mark n) brds
    noMarkC win = [nC c | row <- win, c <- row, not $ bC c]

play2 :: [Int] -> [Board] -> Int
play2 [] _ = error "Nobody wins"
play2 (n:ns) brds
  | length brds == 1 && not (null winners) = n * (sum . noMarkC . head $ winners)
  | null winners = play2 ns newBrds
  | otherwise    = play2 ns (filter (`notElem` winners) newBrds)
  where
    newBrds = map (mark n) brds
    winners = wins newBrds
    noMarkC win = [nC c | row <- win, c <- row, not $ bC c]

wins :: [Board] -> [Board]
wins = filter (\brd -> any (all bC) (brd ++ transpose brd)) where
  transpose brd = [[row !! col | row <- brd] | col <- [0 .. length (head brd) - 1]]

mark :: Int -> Board -> Board
mark n = foldr checkRow [] where
  checkRow r rs = foldr checkCel [] r : rs
  checkCel (C n' b) cs
    | n == n'   = C n' True : cs
    | otherwise = C n' b    : cs

