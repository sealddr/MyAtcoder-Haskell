main = do
    s <- getLine
    putStrLn $ solve s

solve [c1,c2,c3] = if c1 == '7' || c2 == '7' || c3 == '7' then "Yes" else "No"