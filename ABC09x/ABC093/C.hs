import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [a, b, c] <- getIntList
    print . solve . sort $ [a, b, c]
            where solve [a', b', c']
                    | even (c' - a') && even (c' - b') = (c' - a') `div` 2 + (c' - b') `div` 2
                    | even (b' - a') = 1 + (c' - a' - 1) `div` 2 + (c' - b' - 1) `div` 2
                    | even (c' - a') = 1 + (c' - a') `div` 2 + (c' - b' + 1) `div` 2
                    | otherwise = 1 + (c' - a' + 1) `div` 2 + (c' - b') `div` 2
                  solve _ = 0
 