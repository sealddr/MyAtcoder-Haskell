main = do
    _ <- getLine
    a_s <- map read . words <$> getLine
    let miny = minimum a_s
    let maxy = maximum a_s
    print $ solve miny maxy a_s

solve y0 y1 a_s = minimum (map (f a_s) [y0..y1])

f a_s y = foldl (g y) 0 a_s

g y acc x = acc + (x - y) ^ 2