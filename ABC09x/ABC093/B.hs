import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [a, b, k] <- getIntList
    let as = take (fromIntegral k) [a..b]
        bs = take (fromIntegral k) [b, b-1..a]
    putStrLn . unlines . map show . nub . sort $ as ++ bs
