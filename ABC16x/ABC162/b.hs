import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    print $ solve n

solve n = sum $ map fizzbuzz [1..n]

fizzbuzz n
    | isFizz && isBuzz  = 0
    | isFizz = 0
    | isBuzz = 0
    | otherwise = n
    where
        isFizz = n `mod` 3 == 0
        isBuzz = n `mod` 5 == 0
