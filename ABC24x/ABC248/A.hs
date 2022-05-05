main = getLine >>= solve 
solve s = putStrLn $ filter (`notElem` s) ['0'..'9']