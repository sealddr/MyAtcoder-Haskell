main=do
    [n, y] <- map read . words <$> getLine
    putStrLn $ solve n y

solve :: Int -> Int -> String
solve n y  
    | null (res n y) = "-1 -1 -1"
    | otherwise = unwords $ map show (head (res n y))
    where 
    res :: Int -> Int -> [[Int]]
    res n y = [[ne, nu , n-ne-nu]| ne <- [0..n], nu <- [0..(n - ne)], 10000 * ne + 5000 * nu + 1000 * (n-ne-nu) == y] 
