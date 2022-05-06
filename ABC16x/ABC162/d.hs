import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Debug.Trace

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    n <- getInt
    s <- BS.getLine
    print $ solve (fromIntegral n) s

solve n s = rgb s - foldl (f n s) 0 [0..n - 1]
    where rgb s = count 'R' s * count 'G' s * count 'B' s
            where count c s = BS.length $ BS.filter (==c) s
          f n s acc i = acc + foldl (g n s i) 0 [0..n - 1]
          g n s i acc j
            | i >= k i j || j >= k i j = acc 
            | k i j < 0 = acc
            | k i j >= n = acc
            | BS.index s i /= BS.index s j && BS.index s j /= BS.index s (k i j) && BS.index s (k i j) /= BS.index s i = acc + 1
            | otherwise = acc
                where k i j = 2 * j - i



