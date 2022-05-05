main = do
    [x1, y1] <- map (read::String->Int) . words <$> getLine
    [x2, y2] <- map (read::String->Int) . words <$> getLine
    [x3, y3] <- map (read::String->Int) . words <$> getLine
    putStrLn $ show (x x1 x2 x3) ++ " " ++ show( y y1 y2 y3)
        where x x1 x2 x3
                | x1 == x2  = x3
                | x2 == x3  = x1
                | otherwise = x2
              y y1 y2 y3
                | y1 == y2  = y3
                | y2 == y3  = y1
                | otherwise = y2
