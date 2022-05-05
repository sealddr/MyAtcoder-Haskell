import Control.Monad (replicateM)
import Data.List
main = do
    n <- readLn
    ncs <- replicateM n $ head <$> getLine
    print . foldl (\ acc [x,y,z] -> acc + x * y * z) 0 . comb 3 . map length . group . sort $ filter (`elem` "MARCH") ncs

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs
