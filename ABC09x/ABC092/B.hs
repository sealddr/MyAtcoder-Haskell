import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    [d, x] <- getIntList
    as <- replicateM (fromIntegral n) getInt
    print $ x + sum (map (f d) as)
        where f d a = 1 + (d - 1) `div` a