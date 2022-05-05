main = do
    [a, b, k] <- map read . words <$> getLine
    print $ f a b k

f a b k = head [ x | x <- [0..], a * k ^ x >= b]