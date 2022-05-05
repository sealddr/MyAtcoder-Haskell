main = do
    [n,m] <- map read . words <$> getLine
    print $ solve n m

solve 1 1 = 1
solve 1 m = m - 2
solve n 1 = n - 2
solve n m = (n - 2) * (m - 2)
