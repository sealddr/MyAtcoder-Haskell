import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [d, n] <- getIntList
    print $ solve d (fromIntegral n)

solve d n
    | d == 0    = last . take n $ filter (\x -> x `rem` 100 > 0) [1..]
    | d == 1    = last . take n $ filter (\x -> x `rem` 10000 > 0) [100,200..]
    | otherwise = last . take n $ filter (\x -> x `rem` 1000000 > 0) [10000,20000..]