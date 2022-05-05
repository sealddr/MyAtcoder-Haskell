{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (permutations)
check :: Int -> Int -> Int -> [Int] -> Bool
check 3 3 3 [x00, x01, x02, x10, x11, x12, x20, x21, x22] 
    = x00 < x01 && x01 < x02 &&
      x10 < x11 && x11 < x12 && 
      x20 < x21 && x21 < x22 &&
      x00 < x10 && x10 < x20 &&
      x01 < x11 && x11 < x21 && 
      x02 < x12 && x12 < x22

check 3 3 2 [x00, x01, x02, x10, x11, x12, x20, x21] 
    = x00 < x01 && x01 < x02 &&
      x10 < x11 && x11 < x12 && 
      x20 < x21 && 
      x00 < x10 && x10 < x20 &&
      x01 < x11 && x11 < x21 && 
      x02 < x12 

check 3 3 1 [x00, x01, x02, x10, x11, x12, x20] 
    = x00 < x01 && x01 < x02 &&
      x10 < x11 && x11 < x12 && 
      x00 < x10 && x10 < x20 &&
      x01 < x11 && 
      x02 < x12 

check 3 2 2 [x00, x01, x02, x10, x11, x20, x21]
    = x00 < x01 && x01 < x02 &&
      x10 < x11 && 
      x20 < x21 && 
      x00 < x10 && x10 < x20 &&
      x01 < x11 && x11 < x21

check 3 2 1 [x00, x01, x02, x10, x11, x20]
    = x00 < x01 && x01 < x02 &&
      x10 < x11 && 
      x00 < x10 && x10 < x20 &&
      x01 < x11

check 2 2 2 [x00, x01, x10, x11, x20, x21]
    = x00 < x01 && 
      x10 < x11 && 
      x20 < x21 && 
      x00 < x10 && x10 < x20 &&
      x01 < x11 && x11 < x21

check 3 1 1 [x00, x01, x02, x10, x20]
    = x00 < x01 && x01 < x02 &&
      x00 < x10 && x10 < x20

check 2 2 1 [x00, x01, x10, x11, x20]
    = x00 < x01 &&
      x10 < x11 && 
      x00 < x10 && x10 < x20 &&
      x01 < x11

check 2 1 1 [x00, x01, x10, x20] 
    = x00 < x01 && 
      x00 < x10 && x10 < x20 

check 1 1 1 [x00, x10, x20] 
    = x00 < x10 && x10 < x20 

main = do 
    [a0,a1,a2] <- map read . words <$> getLine 
    print . length . filter (check a0 a1 a2) $ permutations [1..(a0+a1+a2)]

