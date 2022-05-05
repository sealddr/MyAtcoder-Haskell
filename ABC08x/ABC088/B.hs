import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    a_s <- getIntList
    print $ solve n a_s

solve n a_s = sum . zipWith (*) (cycle [1,-1]) . reverse $ sort a_s