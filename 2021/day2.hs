part1 :: IO ()
part1 = do
  input <- readFile "input/day2.txt"
  print $ uncurry (*) (route $ formatActions (lines input))

formatActions :: [String] -> [(String,Int)]
formatActions = foldr format [] where
  format act acts = (head $ words act, read $ last $ words act) : acts

route :: [(String,Int)] -> (Int,Int)
route = foldl move (0,0) where
  move (h,d) (action,x)
    | action == "forward" = (h+x,d)
    | action == "down"    = (h,d+x)
    | action == "up"      = (h,d-x)
    | otherwise           = error "Unknown action"

part2 :: IO ()
part2 = do
  input <- readFile "input/day2.txt"
  print $ uncurry (*) (routeAim $ formatActions (lines input))

routeAim :: [(String,Int)] -> (Int,Int)
routeAim instr = (h,d) where
  (h,d,a) = foldl move (0,0,0) instr
  move (h,d,a) (action,x)
    | action == "forward" = (h+x,d+a*x,a)
    | action == "down"    = (h,d,a+x)
    | action == "up"      = (h,d,a-x)
    | otherwise           = error "Unknown action"