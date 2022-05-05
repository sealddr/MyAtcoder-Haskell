import Data.List
main = do
    _ <- getLine
    a_s <- map (read::String -> Int) . words <$> getLine 
    print . foldl f 0 . group $ sort a_s

f :: Int -> [Int] -> Int
f acc x
    | length x > head x = acc + length x - head x
    | length x < head x = acc + length x
    | otherwise = acc  