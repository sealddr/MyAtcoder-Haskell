import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as UV
import Debug.Trace

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n,k]  <- getIntList
    hs <- map fromIntegral <$> getIntList
    let hs' = UV.fromList hs
    print $ solve (fromIntegral n) (fromIntegral k) hs'

solve n k = flip (UV.!) (n - 1) . UV.constructN n . f 
        where f :: UV.Vector Int -> UV.Vector Int -> Int
              f hs dp
                | UV.null dp = 0
                | otherwise = minimum $ step k hs dp
              step k hs dp = map (fm hs dp) [1..min k $ sz dp]
              fm hs dp i = (UV.!) dp (sz dp - i) + dh hs dp i
              dh hs dp i = abs ((UV.!) hs (sz dp) - (UV.!) hs (sz dp - i))
              sz = UV.length