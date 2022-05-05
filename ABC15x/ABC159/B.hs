import Control.Monad
import Data.Maybe

main = do
    s <- getLine
    putStrLn $ solve s

solve :: String  -> String
solve s
   | isPalindrome s && isPalindrome (take (length s `div` 2) s)  && isPalindrome (drop ((length s + 1) `div` 2) s) = "Yes"
   | otherwise = "No"
   where isPalindrome s = s == reverse s
