main = do
    [l,r] <- map read . words <$> getLine
    print $ solve l r

solve :: Int -> Int -> Int
solve l r = sum (map (f . show) [l..r])
f:: String -> Int
f s 
  | length s < 5 = 0 
  | (head s == last s) && (head (tail s) == last (init s)) = 1 
  | otherwise = 0