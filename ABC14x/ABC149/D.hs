import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Debug.Trace

data MyRPS = NOP | WINROCK | WINPAPER | WINSCISSOR | DROWORLOSE
    deriving (Eq, Show, Enum)

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n, k] <- getIntList
    [r,s,p] <- getIntList
    t <- BS.getLine
    print $ solve (fromIntegral n) (fromIntegral k) r s p t
    
solve n k r s p t = M.foldl (+) 0 . M.map (countScore r s p) . M.map (BS.cons 'x'. flip BS.snoc 'x') $ splitByMod n k t

countScore :: Integer -> Integer -> Integer -> BS.ByteString -> Integer
countScore r s p = fst . BS.foldl (fcs r s p) (0, NOP)
    where fcs r s p (acc, myprev) now
              | now == 'x' = (acc, NOP)
              | myprev == WINROCK && now == 's' = (acc, DROWORLOSE)
              | myprev == WINSCISSOR && now == 'p' = (acc, DROWORLOSE)
              | myprev == WINPAPER && now == 'r' = (acc, DROWORLOSE)
              | otherwise = (acc + score r p s now, myRSP now)
                where score r p s now
                       | now == 'r' = p
                       | now == 's' = r
                       | otherwise  = s
                      myRSP now
                       | now == 'r' = WINPAPER
                       | now == 's' = WINROCK
                       | otherwise  = WINSCISSOR

splitByMod n k t = foldl (f k t) (initMap n k) [0..n - 1]

initMap :: Int -> Int -> M.Map Int BS.ByteString
initMap n k = foldl (fim k) M.empty [0..(k - 1)]

fim k mp i = M.insert (i `mod` k) BS.empty mp

f :: Int -> BS.ByteString -> M.Map Int BS.ByteString -> Int -> M.Map Int BS.ByteString
f k t mp i = M.insert (i `mod` k) (BS.cons (BS.index t i) (mp  M.! (i `mod` k))) mp