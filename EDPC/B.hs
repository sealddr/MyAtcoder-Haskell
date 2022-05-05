{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Debug.Trace
type DPTable = [(Int,Int)]

main = do
    [n,k]  <- map read . words <$> getLine
    hs <- map read . words <$> getLine
    print $ solve k hs

solve k (h1:hs) = snd . last $ foldl update (zip (replicate (k - 1) (-1) ++ [h1]) (repeat 0)) hs

update :: DPTable -> Int -> DPTable
update dp h = tail $ dp ++ [foldr (f h) (h, maxBound::Int) dp]

f :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
f h (hi_j, dpi_j) (hi, dpi)
   | hi_j < 0 = (hi, dpi)
   | dpi > dpi_j + abs (hi_j - h) = (hi, dpi_j + abs (hi_j - h))
   | otherwise = (hi, dpi)