main = do
    readLn >>= putStrLn . solve
        where solve n
                  | n < 1000 = "ABC" 
                  | otherwise = "ABD"