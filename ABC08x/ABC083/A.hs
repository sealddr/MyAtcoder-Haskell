main = do
    [a, b, c, d] <- map read . words <$> getLine
    putStrLn $ solve a b c d

solve a b c d =
     case compare (a+b) (c+d) of GT -> "Left"
                                 LT -> "Right"
                                 EQ -> "Balanced"