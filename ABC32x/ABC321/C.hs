import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

readInt = fst . fromJust . BS.readInteger
getInt = readInt <$> BS.getLine

main = do
    k <- getInt
    print . flip (!!) (fromIntegral k - 1) . sort $ map ((read :: [Char]->Integer) . f) [2..(2 ^ 10 - 1)]
            where f :: Integer -> [Char]
                  f x = foldl g [] . reverse $ zip ['9','8'..'0'] (intToPaddedBinaryStr x)
                  g :: [Char] -> (Char, Char) -> [Char]
                  g ans (number, bitmap)
                    | bitmap == '1' = number:ans
                    | otherwise = ans

-- Int型整数を10桁の0埋めされた2進数表現の文字列に変換する関数
intToPaddedBinaryStr :: Integer -> String
intToPaddedBinaryStr n = padTo10Digits (showIntAtBase 2 intToDigit n "")
  where
    padTo10Digits :: String -> String
    padTo10Digits s
      | length s < 10 = replicate (10 - length s) '0' ++ s
      | otherwise = s