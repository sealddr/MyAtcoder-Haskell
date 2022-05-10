import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    s <- replicateM (fromIntegral n) BS.getLine
    m <- getInt
    t <- replicateM (fromIntegral m) BS.getLine
    print $ solve (fromIntegral n) s (fromIntegral m) t


solve n s m t = max 0 . maximum . M.elems $ foldl fm (foldl fp M.empty s) t

fp mp s = M.insertWith (+) s 1 mp
fm mp t = M.insertWith subtract t 1 mp
