main = do
    [a, b] <- map read . words <$> getLine
    s <- getLine
    putStrLn $ solve a b s

solve :: Int -> Int -> String -> String
solve a b s
    | checkA a s && checkH a s && checkB a s && checkL a b s= "Yes"
    | otherwise = "No"
    where checkA a s = all (`elem` numbers) $ take a s
          checkH a s = s !! a == '-'
          checkB a s = all (`elem` numbers) $ drop (a + 1) s
          checkL a b s = a + 1 + b == length s
          numbers = ['0'..'9']