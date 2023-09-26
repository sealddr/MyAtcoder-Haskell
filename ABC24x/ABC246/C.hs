import Data.List
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
main = do
    [_, k, x] <- getIntList
    a_s <- getIntList
    print $ solve k x a_s

solve k x = sum . snd . applyCoupon' x . applyCoupon k x

applyCoupon k x = foldl (f x) (k, [])

f x (k', a_s') a
   | k' == 0          = (k', a:a_s')
   | k' < (a `div` x) = (0, (a - x * k'):a_s')
   | otherwise        = (k'- (a `div` x), (a `mod` x):a_s')

applyCoupon' x (k', a_s') = foldr g (k', []) (sort a_s')

g a' (k'', a_s'')
  | k'' == 0  = (k'', a':a_s'')
  | otherwise = (k''-1, 0:a_s'')