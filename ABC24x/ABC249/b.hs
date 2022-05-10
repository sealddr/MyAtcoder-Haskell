import qualified Data.ByteString.Char8 as BS
import Data.List
main = BS.getLine >>= putStrLn . solve
solve s
    | existsUpperCase s && existsLowerCase s && distinct s = "Yes"
    | otherwise = "No"
        where existsUpperCase s = any (`BS.elem` s) ['A'..'Z']
              existsLowerCase s = any (`BS.elem` s) ['a'..'z']
              distinct s = BS.length s == (length . nub $ BS.unpack s)