main = do
    abc <- map read . words <$> getLine
    k <- readLn
    print $ solve abc k

solve abc@[a,b,c] k
    | a == maximum abc = a * 2 ^ k + b         + c
    | b == maximum abc = a         + b * 2 ^ k + c
    | otherwise        = a         + b         + c * 2 ^ k