main = do
    a <- readLn
    b <- readLn
    c <- readLn
    x <- readLn
    print $ length [() | s <- [0..a], t <- [0..b], u <- [0..c], 500 * s + 100 * t + 50 * u == x]