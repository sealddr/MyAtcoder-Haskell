import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n,_] <- getIntList
    a_s <- getIntList
    print $ solve n a_s

solve :: Integer -> [Integer] -> Integer
solve n a_s = if 0 > ans n a_s then -1 else ans n a_s
    where ans n' as'= n - sum as'
