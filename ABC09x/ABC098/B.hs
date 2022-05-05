main = do
    n <- readLn
    s <- getLine
    print $ solve n s

solve :: Int -> String -> Int
solve n s = maximum $ map (f . (`splitAt` s)) [1..n-1]
f:: (String, String) -> Int
f (s1,s2) = foldl (\acc c -> if c `elem` s1 && c `elem` s2 then acc + 1 else acc) 0 ['a'..'z']