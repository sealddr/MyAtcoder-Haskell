import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n,m] <- getIntList
    print $ solve n m

solve 0 0 = 0
solve n 0 = n * (n - 1) `div` 2
solve 0 m = m * (m - 1) `div` 2
solve n m = n * (n - 1) `div` 2 + m * (m - 1) `div` 2