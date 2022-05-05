cond :: Int -> Int -> Int -> Bool
cond a b x = a <= eval x && eval x <= b
    
eval :: Int -> Int
eval x = sum [ read [c] | c <- show x]

main = do
    [n, a, b] <- map read . words <$> getLine
    print $ sum $ filter (cond a b) [1..n]