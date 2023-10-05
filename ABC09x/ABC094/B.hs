import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n, m, x] <- getIntList
    as <- getIntList
    print $ min (length $ filter (<x) as) (length $ filter (>x) as)