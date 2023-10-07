import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n, x] <- getIntList
    ms <- replicateM (fromIntegral n) getInt
    print $ solve n x ms
        where solve n x ms = n + (x - sum ms) `div` minimum ms