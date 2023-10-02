import Data.List
main = do
    s <- getLine
    putStrLn $ if elem "abc" $ permutations s then "Yes" else "No"