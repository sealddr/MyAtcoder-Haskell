import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as M

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

lowerBoundUV ::  Int -> UV.Vector Int -> Int
lowerBoundUV key vectorUV = binSearch ok ng (pred key vectorUV)
  where ok = UV.length vectorUV
        ng = -1
        pred key vec mid = key <= (vec UV.! mid)

binSearch ::  Int -> Int -> (Int -> Bool) -> Int
binSearch ok ng pred
    | abs (ok - ng) == 1 = ok
    | pred mid = binSearch mid ng pred
    | otherwise = binSearch ok mid pred
    where mid = (ok + ng) `div` 2

main = do
    n <- fromIntegral <$> getInt
    a_s <- UV.cons 0 . UV.fromList . map (fromIntegral::Integer -> Int) <$> getIntList
    let idxs = createIdxs a_s n
    nq <- fromIntegral <$> getInt
    replicateM nq $ getIntList >>= print . handle idxs

handle :: M.Map Int (UV.Vector Int) -> [Integer] -> Int
handle  idxs [l, r, x] 
    | isNothing (M.lookup x' idxs) = 0
    | otherwise = iR x (r' + 1) - iL x l'
    where iR x r'' = lowerBoundUV r'' (fromJust $ M.lookup x' idxs)
          iL x l'' = lowerBoundUV l'' (fromJust $ M.lookup x' idxs)
          l' = fromIntegral l
          r' = fromIntegral r
          x' = fromIntegral x

handle _ _ = error "error: incorrect inputs"

createIdxs a_s n = M.map (UV.fromList . reverse) $ foldl (f a_s) M.empty [1..n]

f :: UV.Vector Int -> M.Map Int [Int] -> Int -> M.Map Int [Int]
f a_s idxs k = M.insert (a_s UV.! k)  (valueM a_s idxs k) idxs
    where valueM a_s' idxs' k'
            | isNothing $ M.lookup (a_s UV.! k') idxs' = [k']
            | otherwise = k' : fromJust (M.lookup (a_s UV.! k') idxs')