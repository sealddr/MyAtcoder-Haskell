import Data.List
main::IO()
main = getContents >>= putStrLn . concat . sort . tail . lines