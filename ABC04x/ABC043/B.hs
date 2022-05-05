main = putStrLn . solve =<< getLine
solve s = reverse $ foldl f "" s

f "" 'B' = ""
f (c:s) 'B' = s
f acc c = c:acc