main = do
    [a,b,c,d] <- map read . words <$> getLine
    putStrLn $ solve a b c d

solve a b c d
    | abs (a - c) <= d = "Yes"
    | abs (a - b) <= d && abs (b - c) <= d = "Yes"
    | otherwise = "No"