import qualified Data.Vector.Unboxed as UV
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

type Cylinder = UV.Vector (Int, Int)

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    n <- getInt
    qs <- map readIntList <$> replicateM (fromIntegral n) BS.getLine
    mapM_ print $ solve qs


solve :: [[Integer]] -> [Int]
solve qs = reverse . fst . handleQ2 (q2s qs) $ handleQ1 (q1s qs)

q1s = filter (\ (i:_) -> i == 1)
q2s = filter (\ (i:_) -> i == 2)

handleQ1 :: [[Integer]] -> Cylinder
handleQ1 q1s = UV.fromList (map (\ [i, x, c] -> (fromIntegral x,fromIntegral c))  q1s)

handleQ2 :: [[Integer]] -> Cylinder -> ([Int], Cylinder)
handleQ2 q2s vecC = foldl f ([], vecC) q2s

f :: ([Int], Cylinder) -> [Integer] -> ([Int], Cylinder)
f (anss, vecC) [_,c] = (ans c' vecC:anss, nextC c' vecC)
   where c' = fromIntegral c

ans :: Int -> Cylinder -> Int
ans c vecC
   | c <= snd h = fst h * c
   | otherwise = uncurry (*) h + ans (c - snd h) (UV.tail vecC)
   where h = UV.head vecC

nextC :: Int -> Cylinder -> Cylinder
nextC c vecC
   | c <= snd h = (fst h,snd h - c) `UV.cons` UV.tail vecC
   | otherwise = nextC (c - snd h) (UV.tail vecC)
   where h = UV.head vecC

