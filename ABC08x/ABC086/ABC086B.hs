main :: IO ()
main = do
    n <- read . concat . words <$> getLine
    putStrLn $ if any (check n) [1..1000] then "Yes" else "No"

check x i = x == i * i