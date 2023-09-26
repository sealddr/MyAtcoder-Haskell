main = do
    s <- getLine
    putStrLn $ solve s 1

solve :: String -> Int -> String
solve (c0:c1:c2:s) i
    | c0 == c1 = unwords $ map show [i, i+1]
    | c0 == c2 = unwords $ map show [i, i+2]
    | otherwise = solve (c1:c2:s) (i+1)

solve [c0,c1] i = if c0 == c1 then unwords $ map show [i, i+1] else "-1 -1"
solve[_] _ = "-1 -1"
solve[] _ = "-1 -1"