s :: Int -> String 
s 1 = "1"
s n = s (n - 1) ++ " " ++ show n ++ " " ++ s (n - 1)

main :: IO()
main = do
    n <- read <$> getLine
    putStrLn $ s n