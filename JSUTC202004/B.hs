import Control.Monad
import Data.List
main=do
    n <- readLn
    balls <- replicateM n $ words <$> getLine
    putStr . solve $ map ( \ [x,[c]] -> ((read::String->Int) x, c) ) balls

solve balls =  unlines $ map show (red balls ++ blue balls)
blue balls = sort . map fst $ filter (\ (_,c) -> c=='B') balls
red balls = sort . map fst $ filter (\ (_,c) -> c=='R') balls
