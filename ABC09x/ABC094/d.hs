import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List
import Debug.Trace

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    a_s <- getIntList
    putStrLn $ solve n a_s

solve :: Integer -> [Integer] -> String
solve n a_s = show (maximum a_s) ++ " " ++ show (chooseR a_s)
    where chooseR a_s = foldl (f (maximum a_s)) 0 (init $ sort a_s)
          f::Integer -> Integer -> Integer -> Integer 
          f maxa acc a 
              | abs (maxa - 2 * a) < abs (maxa - 2 * acc) = a 
              | otherwise = acc