main = do
    [a,b] <- map read . words <$> getLine
    print $ (b-a) * (b - a +1) `div` 2 - b