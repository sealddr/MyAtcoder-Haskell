main = do
    n <- readLn
    s <- getLine
    putStrLn $ if n < length s then foldr (:) "..." $ take n s else s