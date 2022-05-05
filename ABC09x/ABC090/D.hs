main = do
    [n,k] <- map read . words <$> getLine
    print $ solve n k

solve n k = sum $ map (f n k) [0..(n-1)]
    where f n 0 0 = n
          f n k 0 = n - k
          f n 0 b = max 0 b * (n `div` b) + max 0 (n `mod` b + 1) - 1
          f n k b = max 0 (b - k) * (n `div` b) + max 0 (n `mod` b + 1 - k)