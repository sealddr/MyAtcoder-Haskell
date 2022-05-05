import Data.List
import Data.Maybe

main = do
    s <- getLine
    putStrLn $ solve s

solve = makeSol . solve'

solve' s
    | null sol = Nothing
    | otherwise = head sol
    where sol = dropWhile isNothing $ map (f s) ['a'..'z']

makeSol ::Maybe (Int,Int) -> String
makeSol x = if isNothing x then "-1 -1" else show (snd $ fromJust x) ++ " " ++ show (uncurry (+) (fromJust x) - 1)

f s c = check (length s) $ repl s c
repl s toc = map (\ c -> if c == toc then '1' else '0' ) s

check :: Int -> String -> Maybe (Int, Int)
check _ [] = Nothing
check _ [c] = Nothing

check n [c0, c1]
    | [c0, c1] == "11" = Just (2, n-1)
    | otherwise        = Nothing

check n (c0:c1:c2:s)
    | [c0, c1] == "11" = Just (2, n - 2 - length s)
    | [c1, c2] == "11" = Just (2, n - 1 - length s)
    | [c0, c2] == "11" = Just (3, n - 2 - length s)
    | c2    == '1'  = check n (c2:s)
    | c1    == '1'  = check n (c1:c2:s)
    | otherwise     = check n s