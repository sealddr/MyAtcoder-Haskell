main = do
    x <- readLn 
    a <- readLn 
    b <- readLn
    print $ let n = (x - a)  `div` b in x -a - n * b