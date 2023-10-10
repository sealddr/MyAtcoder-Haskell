import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
    [n, m] <- getIntList
    a_s <- getIntList
    s_s <- replicateM (fromIntegral n) getLine

    let now_scores::[Int]
        now_scores = map (calc_now_score a_s s_s) [1..(fromIntegral n)]
               where calc_now_score::[Integer] -> [String] -> Int -> Int
                     calc_now_score a_s s_s i =
                        (+) i . sum . zipWith (*) (map fromIntegral a_s) $ map (\c -> if c=='o' then 1 else 0) $ s_s !! (i-1)

    let max_score = maximum now_scores

    let rest_problems::[[Int]]
        rest_problems = map (calc_rest_problem a_s s_s) [1..(fromIntegral n)]
                where calc_rest_problem::[Integer] -> [String] -> Int -> [Int]
                      calc_rest_problem a_s s_s i =
                            sort . filter (>0) . zipWith (*) (map fromIntegral a_s) $ map (\c -> if c=='x' then 1 else 0) $ s_s !! (i-1)

    mapM_ (print . solve now_scores rest_problems max_score) [1..(fromIntegral n)]

solve :: [Int] -> [[Int]] -> Int -> Int -> Int
solve now_scores rest_problems max_score i =
    getFirstElement . foldr f (0, (!!) now_scores (i-1), max_score) $ (!!) rest_problems (i-1)

f :: Int -> (Int, Int, Int) -> (Int, Int, Int)
f rp (ans, now_score, max_score) =
        if now_score >= max_score then (ans, now_score, max_score)
        else (ans + 1, now_score + rp, max_score)

-- トリプルから第一要素を取得する関数
getFirstElement :: (a, b, c) -> a
getFirstElement (x, _, _) = x
