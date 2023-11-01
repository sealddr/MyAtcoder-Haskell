import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    _ <- getInt
    (a:a_s) <- getIntList
    putStrLn $ if all (== a) a_s then "Yes" else "No"