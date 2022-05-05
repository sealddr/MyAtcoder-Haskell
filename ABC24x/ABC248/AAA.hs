import Data.Char
main = getLine >>= print . foldr (subtract . digitToInt) 45