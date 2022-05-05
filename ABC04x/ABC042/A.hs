main = getLine >>= solve . map read . words

solve x
    | x==[5,5,7] = putStrLn "Yes"
    | x==[5,7,5] = putStrLn "Yes"
    | x==[7,5,5] = putStrLn "Yes"
    | otherwise = putStrLn "No"
