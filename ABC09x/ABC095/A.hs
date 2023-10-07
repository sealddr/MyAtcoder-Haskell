main = do
    s <- getLine
    putStrLn $ solve s
        where solve s
                | s =="ooo" = "1000"
                | s =="oox" = "900"
                | s =="oxo" = "900"
                | s =="xoo" = "900"
                | s =="oxx" = "800"
                | s =="xox" = "800"
                | s =="xxo" = "800"
                | otherwise = "700"