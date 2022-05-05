import Data.List ( group, sort )
main = do
    [n,k] <- map read . words <$> getLine
    a <- map (read::String->Int) . words <$> getLine
    print $ sum $ drop k $ reverse $ sort $ map length $ group $ sort a