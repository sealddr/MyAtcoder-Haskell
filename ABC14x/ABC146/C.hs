import Data.Maybe
main = do
    [a,b,x] <- map read . words <$> getLine
    putStrLn $ maybe "0" show $ solve a b x

solve :: Int -> Int -> Int -> Maybe Int
solve a b x
    | sol == 0 = Nothing
    | otherwise = Just sol
    where sol = binSearch' ok ng (check a b x)
          ok = 0
          ng = 10 ^ 9 + 1
          check a b x = \ n -> a * n + b * d n <= x
          d n
            | n < 10 = 1
            | otherwise = 1 + d (n `div` 10)

binSearch' ::  Int -> Int -> (Int -> Bool) -> Int
binSearch' ok ng check
    | abs (ok - ng) == 1 = ok
    | check mid = binSearch' mid ng check
    | otherwise = binSearch' ok mid check
    where mid = (ok + ng) `div` 2