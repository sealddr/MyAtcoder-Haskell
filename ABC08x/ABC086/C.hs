import Control.Monad

main = do
    n <- readLn
    xs <- replicateM n (map (read::String->Int) . words <$> getLine)
    putStrLn $ solve xs

solve :: [[Int]] -> String
solve xs
    | possible (0, 0, 0) (plans xs) = "Yes"
    | otherwise   = "No"
    where possible :: (Int,Int,Int) -> [(Int,Int,Int)] -> Bool
          possible _ [] = True
          possible now (next:ps) = checkNext now next && possible next ps
          plans :: [[Int]] -> [(Int,Int,Int)]
          plans ps = map (\[t, x, y] -> (t, x, y)) ps
          checkNext :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
          checkNext (t0, x0, y0) (t, x, y) = condT && condXY 
            where condT = dist <= erapsedTime 
                  condXY = even (erapsedTime - dist)
                  dist = abs (x0-x) + abs (y0-y)
                  erapsedTime = t - t0                   