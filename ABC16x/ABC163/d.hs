import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Debug.Trace

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n,k] <- map fromIntegral <$> getIntList
    print $ solve n k;

solve n k = foldl (f n) 0 [k..(n + 1)]
    where f n acc i = flip mod diviser $ acc + sum' (n - i + 1) n - sum' 0 (i - 1) + 1
          sum' l r = (r - l + 1) * (r + l) `div` 2
          diviser = 10 ^ 9 + 7
