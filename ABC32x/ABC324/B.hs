import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    n <- getInt
    putStrLn $ if solve n then "Yes" else "No"
            where solve n = n `elem` ([2 ^ x * 3 ^ y | x <- [0..60], y <- [0..42], 2 ^ x * 3 ^ y <= 10 ^ 18])