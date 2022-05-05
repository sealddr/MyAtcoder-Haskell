import Data.List
main = do
    s <- getLine
    t <- getLine
    putStrLn $ if sort s < reverse (sort t) then "Yes" else "No"