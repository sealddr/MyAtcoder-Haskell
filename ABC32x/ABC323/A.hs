import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    s <- getLine
    let evenIndex = getEvenIndex s
        allZeros = all (== '0') evenIndex
    if allZeros then putStrLn "Yes" else putStrLn "No"

getEvenIndex :: [a] -> [a]
getEvenIndex [] = []
getEvenIndex [_] = []
getEvenIndex (x:y:xs) = y : getEvenIndex xs