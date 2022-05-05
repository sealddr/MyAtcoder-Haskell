main = do
    n <- readLn
    a_s <- map read . words <$> getLine
    print $ sum a_s - n