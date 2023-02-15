main = do
  [a,b,c] <- map read . words <$> getLine
  putStrLn $ ans a b c
  
ans :: Int -> Int -> Int -> String
ans a b c
    | a <= b && b <= c = "Yes"
    | c <= b && b <= a = "Yes"
    | otherwise        = "No"