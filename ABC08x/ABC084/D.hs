import Control.Monad (replicateM)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine
like2017 :: M.Map Int () -> Int -> Int
like2017 pm n
   | even n = 0
   | M.member n pm && M.member ((n + 1) `div` 2) pm = 1
   | otherwise = 0

primeMap :: [Int] -> M.Map Int ()
primeMap = foldl (\ pm p -> M.insert p () pm) M.empty

primes :: Int -> [Int]
primes n
    | n < 2 = []
    | n == 2 = [2]
    | otherwise = sieve [x | x <- [2..n]] n
    where
        sieve :: [Int] -> Int -> [Int]
        sieve [] _ = []
        sieve (x:xs) n
            | x^2 > n = x:xs
            | otherwise = x : sieve [a | a <- xs, a `mod` x > 0] n

main = do
   n <- getInt
   qs <- getIntNList n
   let pm = primeMap $ primes 100000
   let ansv = UV.tail . UV.scanl (+) 0 $ UV.map (like2017 pm) (UV.generate 100000 id)
   mapM_ (print . (\ [l, r] -> (ansv UV.! fromInteger r) - (ansv UV.! fromInteger (l-1)) )) qs
