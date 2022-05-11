import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as UV
readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine
main = getInt >>= print . solve . fromIntegral
solve :: Int -> Int
solve n = flip (UV.!) n $ UV.constructN (n+1) (fc n)
    where fc::Int -> UV.Vector Int -> Int
          fc n vec 
            | UV.length vec == 0 = 0
            | otherwise = (+1) . minimum $ mapMaybe (fm vec (UV.length vec)) (coins n)
          coins n = 1:coinsk 6 n ++ coinsk 9 n
          coinsk k n = takeWhile (<=n) . scanl1 (*) $ repeat k
          fm vec i coin
            | i < coin = Nothing 
            | otherwise = Just $ (UV.!) vec (i - coin) 