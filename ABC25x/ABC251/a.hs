
main = do
    s <- getLine
    let solve s
          | length s == 1 = replicate 6 s
          | length s == 2 = replicate 3 s
          | otherwise = replicate 2 s
    putStrLn . concat $ solve s