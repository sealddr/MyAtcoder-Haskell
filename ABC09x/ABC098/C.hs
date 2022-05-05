import Debug.Trace
main = do
    _ <- getLine
    s <- getLine
    print $ solve s

solve s = minimum $ zipWith (+) (sel s) (swr s)

sel s = init $ scanl (\ acc x -> if x =='W' then acc + 1 else acc ) 0 s
swr s = tail $ scanr (\ x acc -> if x =='E' then acc + 1 else acc ) 0 s