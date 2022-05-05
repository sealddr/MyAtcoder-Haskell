main = do 
    n <- readLn
    putStrLn $ (\ k -> if k `elem` [2,4,5,7,9] then "hon" else if k `elem` [0,1,6,8] then "pon" else "bon" ) (n `mod` 10)