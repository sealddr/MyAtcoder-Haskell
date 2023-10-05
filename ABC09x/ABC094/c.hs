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
    as <- getIntList
    mapM_ print $ solve (fromIntegral n) as
        where solve n as = solve' as (ml n as) (mr n as)
              ml n as = flip (!!) ((n - 1) `div` 2) $ sort as
              mr n as = flip (!!) ((n + 1) `div` 2) $ sort as
              solve' as ml mr = map (\a -> if a <= ml then mr else ml) as