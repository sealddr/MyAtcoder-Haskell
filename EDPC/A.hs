{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type DPTable = (Int, Int)
type Acc = (DPTable, Int, Int)

main = do
    _ <- getLine
    hs <- map read . words <$> getLine
    print $ solve hs

solve [h1] = 0
solve (h1:h2:hs) = getAns $ foldl update ((0, abs (h1-h2)), h1, h2) hs

getAns :: Acc -> Int 
getAns ((_, ans), _, _) = ans

update :: Acc -> Int -> Acc
update ((dpi_2, dpi_1), h_2, h_1) h = 
    (
        (dpi_1, min (dpi_2 + abs (h_2 - h)) (dpi_1 + abs (h_1 - h))),
        h_1,
        h
    )