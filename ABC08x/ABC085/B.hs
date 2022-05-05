import Data.List
main = do
    c <- getContents
    print $ length $ map (const ()) $ group $ sort $ tail $ lines c