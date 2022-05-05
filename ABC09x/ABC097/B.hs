main = do
    x <- readLn
    print . maximum $ filter (<=x) [b ^ p | b <- [1..1000], p <-[2..10]]