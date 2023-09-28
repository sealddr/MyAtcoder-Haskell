import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main :: IO ()
main = do
    [n, x] <- getIntList
    as <- getIntList
    let ans = length $ filter (< x) $ map (f as) [0..100]
                  where f :: [Integer] -> Integer -> Integer
                        f as a = g $ sort (a:as)
                        g :: [Integer] -> Integer
                        g as = sum . tail $ init as
    print $ if ans > 100 then -1 else ans
