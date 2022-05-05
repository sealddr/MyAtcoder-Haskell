import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

tuplify2 (x:y:_) = (x,y)
tuplify2 _ = undefined
readInt = fst . fromJust . BS.readInteger
readIntTuple = tuplify2 . map readInt . BS.words
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getIntNTuples n = map readIntTuple <$> replicateM (fromIntegral n) BS.getLine

main = do
    [n,h] <- getIntList
    ab_s <- getIntNTuples n
    print $ solve n h ab_s

solve n h ab_s = wieldA (chooseA ab_s) . throwBs h . sort $ chooseBs (chooseA ab_s) ab_s
    where chooseA ab_s = maximum $ map fst ab_s
          chooseBs a ab_s = filter (>a) $ map snd ab_s
          throwBs h bs =  foldr throw (h, 0) bs
          throw b (hp, ans)
            | hp <= 0 = (hp, ans)
            | otherwise = (hp-b, ans + 1)
          wieldA a (hp, ans)
            | hp <= 0 = ans
            | otherwise = ans + (hp + a - 1) `div` a