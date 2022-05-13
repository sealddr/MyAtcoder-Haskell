import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n,_] <- getIntList
    getIntList >>= print . solve n

solve :: Integer -> [Integer] -> Integer
solve n a_s = if 0 > n - sum a_s then -1 else n - sum a_s
