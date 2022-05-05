main = do
    _ <- getLine
    ts <- map (read::String -> Int) . words <$> getLine
    print $ foldl f 0 ts

f a b = if y `mod` (2 ^ (b + 1)) == 0 then y + 2 ^ b else y
    where y = (a `div` (2 ^ b) + 1) * 2 ^ b