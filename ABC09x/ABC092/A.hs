import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    a <- getInt
    b <- getInt
    c <- getInt
    d <- getInt
    print $ min a b + min c d