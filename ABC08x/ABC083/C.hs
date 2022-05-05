main = do
    [x, y] <- map read . words <$> getLine
    print $ length [ () | k <- [0..1000], x * 2 ^ k <= y ]