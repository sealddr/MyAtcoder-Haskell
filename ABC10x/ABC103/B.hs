main = do
    s <- getLine
    t <- getLine
    putStrLn $ solve s t

solve :: Eq a => [a] -> [a] -> [Char]
solve s t = if s == t || any (check s t) [1..(n s-1)] then "Yes" else "No"
    where check s t i = take i s == drop (n s - i) t && take (n s - i) t == drop i s  
          n = length
