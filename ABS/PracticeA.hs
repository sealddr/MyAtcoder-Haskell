import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    a <- getInt
    [b,c] <- getIntList
    s <- BS.getLine
    let abc = BS.pack $ show (a+b+c)
    BS.putStrLn $ BS.append  abc (' ' `BS.cons` s)