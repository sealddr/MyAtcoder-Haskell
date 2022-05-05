import Data.List
main = getLine >>= print . foldr (subtract . read) 45 . Data.List.group