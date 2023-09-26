import Data.List

main = do
    n <- getLine
    putStrLn $ if n `isSubsequenceOf` ['9','8'..'0'] then "Yes" else "No"