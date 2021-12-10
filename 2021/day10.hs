import Data.Maybe
import Data.List

part1 :: IO ()
part1 = do
  input <- readFile "input/day10.txt"
  print . syntaxErrorScore . lines $ input

part2 :: IO ()
part2 = do
  input <- readFile "input/day10.txt"
  let scores = sort . completionScore . lines $ input
  print $ scores !! div (length scores) 2

invert :: Char -> Char
invert c = fromMaybe (error "Not a bracket") (lookup c invertTable) where
  invertTable = [('(',')'),(')','('),('[',']'),(']','['),('{','}'),('}','{'),('<','>'),('>','<')]

syntaxErrorScore :: [String] -> Int
syntaxErrorScore s = foldl addScore 0 (map illegalChar s) where
  addScore score c = maybe score (score +) (lookup c scores)
  scores = [(')',3),(']',57),('}',1197),('>',25137)]

completionScore :: [String] -> [Int]
completionScore s = [foldl addScore 0 ln | ln <- filter (/= "") $ map completion s] where
  addScore score c = maybe score (5*score +) (lookup c scores)
  scores = [(')',1),(']',2),('}',3),('>',4)]

illegalChar :: String -> Char
illegalChar s = scanString s ['$'] where
  scanString (c:cs) stack@(pop:stack')
    | c `elem` "([{<" = scanString cs (c:stack)
    | c == invert pop = scanString cs stack'
    | otherwise       = c
  scanString [] []    = error "No illegal char"
  scanString [] stack = '*'
  scanString s  []    = error "Stack not initialized"

completion :: String -> String
completion s = scanString s ['$'] where
  scanString (c:cs) stack@(pop:stack')
    | c `elem` "([{<" = scanString cs (c:stack)
    | c == invert pop = scanString cs stack'
    | otherwise       = ""
  scanString [] []    = error "No completion"
  scanString [] stack = map invert (init stack)
  scanString s  []    = error "Stack not initialized"

