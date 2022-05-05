import Data.List
main = do
    s <- getLine
    k <- readLn
    putStrLn $ (!!) (substring s) k
    where substring  s = sort . nub . concatMap inits . nub . map (take 5) . take 5 . sort . nub. map (take 5) .init $ tails s