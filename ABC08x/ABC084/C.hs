import Control.Monad
main = do
    n <- readLn
    trains <- replicateM (n-1) $ map read . words <$> getLine
    putStrLn . unlines . map show $ solve trains

solve :: [[Int]] -> [Int]
solve [] = [0]
solve trains@(t:ts) = foldl func 0 trains : solve ts
    where func :: Int -> [Int] -> Int
          func acc [c, s, f]
             | acc < s =  s + c
             | acc `mod` f == 0 = acc + c
             | otherwise = acc + f - (acc `mod` f) + c