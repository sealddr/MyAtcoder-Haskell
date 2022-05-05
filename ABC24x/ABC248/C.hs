import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as UV
type DPTable = UV.Vector (Int, Int)

diviser = 998244353
modPlus a b = ((a `mod` diviser) + (b `mod` diviser)) `mod` diviser

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
    [n,m,k] <- getIntList
    print .UV.foldl modPlus 0 $ solve n m k

solve :: Integer -> Integer -> Integer -> UV.Vector Int
solve n m k = foldl (f m' k') (1 `UV.cons` UV.replicate (k' - 1) 0) [1..n]
    where m' = fromIntegral m
          k' = fromIntegral k

f :: Int -> Int -> UV.Vector Int -> Integer -> UV.Vector Int
f m k dp _ = foldl (g dp m) (UV.singleton 0) [1..k]

g dp m dp' k' = dp' `UV.snoc` foldl (h dp k') 0 [1..m]

h dp k' ans m'
  | k'- m' < 0 = ans
  | otherwise = ans `modPlus` (dp UV.! (k'- m'))
  