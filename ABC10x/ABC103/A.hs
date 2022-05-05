import Data.List (permutations)
main = do
    getLine >>= print . solve . map read . words

solve = minimum . map f . permutations
        where f [a1, a2, a3] = abs (a1 - a2) + abs (a2 - a3)
              f _ = undefined 