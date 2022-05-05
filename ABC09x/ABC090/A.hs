main = do
    [c11,   _,   _] <- getLine
    [  _, c22,   _] <- getLine
    [  _,   _, c33] <- getLine
    putStrLn [c11, c22, c33]