main = do
    [x,y,z] <- map read . words <$> getLine
    putStrLn $ ans x y z 
        where ans :: Int -> Int -> Int -> String
              ans x y z = unwords $ map show [z,x,y]