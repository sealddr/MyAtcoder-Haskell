main = do
    [s,l,r] <- map read . words <$> getLine
    print $ solve s l r

solve :: Int -> Int -> Int -> Int
solve s l r
    | s < l = l
    | r < s = r
    | otherwise = s