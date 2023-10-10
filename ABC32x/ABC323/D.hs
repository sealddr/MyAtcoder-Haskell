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
    slimes <- replicateM (fromIntegral n) getIntList
    print . solve . M.fromList $ map (\[s, c]-> (s,c)) slimes

solve::M.Map Integer Integer -> Integer
solve slimes
    | M.null slimes = 0
    | otherwise = let (s, c) = M.findMin slimes
                      ns = 2 * s
                      nc = c `div` 2
                      slimes' = M.deleteMin slimes
                      slimes''
                         | M.member ns slimes' = M.adjust (+nc) ns slimes' 
                         | nc > 0              = M.insert ns nc slimes' 
                         | otherwise           = slimes'
                  in (c `rem` 2) + solve slimes''