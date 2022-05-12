import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    abcs <- replicateM (fromIntegral n) getIntList
    let f [ha, hb, hc] [a, b, c] = [max (hb + a) (hc + a), max(hc + b) (ha + b), max(ha + c) (hb + c)]
    print . maximum . foldl f (head abcs) . map (map fromIntegral) $ tail abcs
