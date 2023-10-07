import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [a, b, c, x, y] <- getIntList
    print $ solve a b c x y
        where solve a b c x y = minimum . filter (>= 0) $ map (\k -> f k a b c x y) [0..3]
              f 0 a b c x y = a * x + b * y
              f 1 a b c x y = if y >= x then c * 2 * x + b * (y - x) else -1
              f 2 a b c x y = if x >= y then c * 2 * y + a * (x - y) else -1
              f 3 a b c x y = c * 2 * max x y
              f _ a b c x y = 0