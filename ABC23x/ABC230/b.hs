import Data.List
main = getLine >>= putStrLn . \s -> if s `isInfixOf` t then "Yes" else "No"
    where t = take 100 $ cycle "oxx"