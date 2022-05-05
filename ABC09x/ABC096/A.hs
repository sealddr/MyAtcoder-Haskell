main = do
    [a, b] <- map read . words <$> getLine
    print $ if a > b then a-1 else a