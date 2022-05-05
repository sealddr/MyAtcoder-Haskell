numShift :: Int -> Int 
numShift x
    | odd x = 0
    | otherwise  = 1 + numShift (x `div` 2)

main = do
    _ <- getLine
    a <- map read . words <$> getLine
    print $ minimum [numShift x| x <- a ]