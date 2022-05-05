main = print . solve =<< readLn
solve n = n * (n+1) `div` 2