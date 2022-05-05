import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Debug.Trace
type Ans = (Integer, Integer, Integer)
readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n, x, y] <- getIntList
    a_s <- getIntList
--    print .(\ (ans,_,_) -> ans) $ solve n x y a_s
    print $ solve' n x y a_s

solve' n x y a_s = scanl (f x y) (0, 0, 0) (zip [1..n] a_s)
solve n x y a_s = foldl (f x y) (0, 0, 0) (zip [1..n] a_s)

f :: Integer -> Integer -> Ans -> (Integer, Integer) -> Ans
f x y (ans, 0, 0) (i,a)
    | x == a    = (ans, i, 0)
    | y == a    = (ans, 0, i)
    | otherwise = (ans, 0, 0)

f x y (ans, idx, 0) (i,a)
    | x < a     = (ans, 0, 0)
    | y > a     = (ans, 0, 0)
    | y == a    = (ans + comb2 (i - idx + 1), 0, i)
    | otherwise = (ans, idx, 0)
        where comb2 k = trace (show i ++ " " ++ show idx) k * (k-1) `div` 2

f x y (ans, 0, idy) (i,a)
    | x < a     = (ans, 0, 0)
    | y > a     = (ans, 0, 0)
    | x == a    = (ans + comb2 (i - idy + 1), i, 0)
    | otherwise = (ans, 0, idy)
        where comb2 k = trace (show i ++ " " ++ show idy) k * (k-1) `div` 2

f x y (ans, idx, idy) (i,a)
    | x == a    = (ans, i, 0)
    | y == a    = (ans, i, 0)
    | otherwise = (ans + comb2 (abs idx-idy + 1), 0, 0)
        where comb2 k = trace (show i ++ " " ++ show idx) k * (k-1) `div` 2
