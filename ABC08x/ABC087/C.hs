main = do
    n <- readLn 
    a1s <- map read . words <$> getLine
    a2s <- map read . words <$> getLine
    print . maximum . map sum $ allRoute n a1s a2s
allRoute:: Int -> [Int] -> [Int] -> [[Int]]
allRoute n a1s a2s = map (\ i -> take (i + 1) a1s ++ drop i a2s) [0.. n - 1]