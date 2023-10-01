import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n, m] <- getIntList
    a_s <- getIntList
    let ans = reverse . fst $ foldl f ([], a_s) [1..n]
    mapM_ print ans
        where f :: ([Integer], [Integer]) -> Integer -> ([Integer], [Integer])
              f (ans, a:a_s) i = if a < i then f (ans, a_s) i else ((a-i):ans,a:a_s)
              f (ans, []) _ = (ans, [])