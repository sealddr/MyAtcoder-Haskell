main = do
    _ <- getLine
    ss <- filter (/=' ') <$> getLine 
    putStrLn $ if 'Y' `elem` ss then "Four" else "Three"